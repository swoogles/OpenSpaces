package co.wtf.openspaces.db

import co.wtf.openspaces.*
import co.wtf.openspaces.discussions.{DiscussionAction, DiscussionActionConfirmed, DiscussionState}
import neotype.unwrap
import zio.*
import zio.json.*

/** Persistent discussion storage using event sourcing.
  * 
  * Flow:
  * 1. Action comes in → record event → update materialized state → return confirmed action
  * 2. On startup → load state from discussions table
  */
class PersistentDiscussionStore(
  eventRepo: EventRepository,
  discussionRepo: DiscussionRepository,
  topicVoteRepo: TopicVoteRepository,
  userRepo: UserRepository,
  gitHubProfileService: GitHubProfileService,
  glyphiconService: GlyphiconService,
  state: Ref[DiscussionState]
) extends DiscussionStore:

  /** Maximum number of topics allowed. Protects against runaway topic creation. */
  private val MaxTopicCount = 200
  
  /** Minimum number of topics to maintain during chaos mode. */
  private val MinTopicCount = 5

  def snapshot: UIO[DiscussionState] = state.get

  /** Apply a confirmed action to the in-memory state (e.g., SlackThreadLinked) */
  def applyConfirmed(action: DiscussionActionConfirmed): UIO[Unit] =
    state.update(_.apply(action))

  /** Process an action: persist event, update materialized view, return confirmed action */
  def applyAction(action: DiscussionAction): Task[DiscussionActionConfirmed] =
    val actor = extractActor(action)
    applyActionWithActor(action, actor)

  def randomDiscussionAction: Task[DiscussionActionConfirmed] =
    for
      currentState <- state.get
      topicCount = currentState.data.size
      noCurrentItems = topicCount == 0
      belowMinimum = topicCount < MinTopicCount
      atCapacity = topicCount >= MaxTopicCount
      action <- if noCurrentItems || belowMinimum then
        // Always add when below minimum to prevent death spiral
        randomAddAction
      else if atCapacity then
        // At capacity - only do non-add actions
        for
          actionIdx <- Random.nextIntBounded(8)
          action <- actionIdx match
            case 0 | 1 | 2 | 3 => randomVoteAction(currentState)
            case 4 | 5 | 6 => randomNotInterestedVoteAction(currentState)
            case 7 => randomScheduleAction(currentState)
        yield action
      else
        for
          actionIdx <- Random.nextIntBounded(10)
          action <- actionIdx match
            case 0 => randomAddAction
            // case 1 => randomDeleteAction(currentState)
            case 1 | 2 | 3 | 4 => randomVoteAction(currentState)
            case 5 | 6 | 7 => randomNotInterestedVoteAction(currentState)
            // case 8 => randomRenameAction(currentState)
            case 8 | 9 => randomScheduleAction(currentState)
        yield action
      result <- applyAction(action)
    yield result

  // Pool of real GitHub users for random action generation
  private def randomPerson: UIO[Person] =
    RandomUsers.randomPerson

  private def randomAddAction: Task[DiscussionAction] =
    for
      person <- randomPerson
      topic <- DiscussionTopics.randomTopic
    yield DiscussionAction.Add(topic, person)

  private def randomDeleteAction(currentState: DiscussionState): Task[DiscussionAction] =
    for
      topicIdOpt <- randomExistingTopicId(currentState)
      person <- randomPerson
      topic <- DiscussionTopics.randomTopic
    yield topicIdOpt match
      case Some(topicId) => DiscussionAction.Delete(topicId)
      case None => DiscussionAction.Add(topic, person)

  private def randomVoteAction(currentState: DiscussionState): Task[DiscussionAction] =
    for
      topicIdOpt <- randomExistingTopicId(currentState)
      person <- randomPerson
      topic <- DiscussionTopics.randomTopic
    yield topicIdOpt match
      case Some(topicId) => DiscussionAction.Vote(topicId, Feedback(person, VotePosition.Interested))
      case None => DiscussionAction.Add(topic, person)

  private def randomNotInterestedVoteAction(currentState: DiscussionState): Task[DiscussionAction] =
    for
      topicIdOpt <- randomExistingTopicId(currentState)
      person <- randomPerson
      topic <- DiscussionTopics.randomTopic
    yield topicIdOpt match
      case Some(topicId) => DiscussionAction.Vote(topicId, Feedback(person, VotePosition.NotInterested))
      case None => DiscussionAction.Add(topic, person)

  private def randomRenameAction(currentState: DiscussionState): Task[DiscussionAction] =
    for
      topicIdOpt <- randomExistingTopicId(currentState)
      newTopic <- DiscussionTopics.randomTopic
      person <- randomPerson
    yield topicIdOpt match
      case Some(topicId) => DiscussionAction.Rename(topicId, newTopic)
      case None => DiscussionAction.Add(newTopic, person)

  private def randomScheduleAction(currentState: DiscussionState): Task[DiscussionAction] =
    for
      topicIdOpt <- randomExistingTopicId(currentState)
      person <- randomPerson
      topic <- DiscussionTopics.randomTopic
      slots = currentState.slots
      // Find an available slot
      availableSlot = slots
        .flatMap(daySlot =>
          daySlot.slots.flatMap(timeSlotForAllRooms =>
            timeSlotForAllRooms.rooms
              .find(room =>
                !currentState.data.values.exists(discussion =>
                  discussion.roomSlot.contains(RoomSlot(room, timeSlotForAllRooms.time))
                )
              )
              .map(room => RoomSlot(room, timeSlotForAllRooms.time))
          )
        )
        .headOption
    yield topicIdOpt match
      case None => 
        // No topics exist, create one
        DiscussionAction.Add(topic, person)
      case Some(topicId) =>
        availableSlot match
          case Some(roomSlot) =>
            DiscussionAction.SetRoomSlot(
              topicId,
              currentState.data.get(topicId).flatMap(_.roomSlot),
              Some(roomSlot),
            )
          // No slots available - vote instead of deleting to avoid death spiral
          case None => DiscussionAction.Vote(topicId, Feedback(person, VotePosition.Interested))

  /** Random schedule-only action: move to slot, unschedule, or swap.
    * Does not create/delete topics or change votes.
    * Heavily biased towards scheduling and swapping (95%), unscheduling rare (5%).
    */
  def randomScheduleAction: Task[DiscussionActionConfirmed] =
    for
      currentState <- state.get
      topics = currentState.data.values.toList
      result <- if topics.isEmpty then
        // No topics to schedule - return a no-op (will be rejected gracefully)
        ZIO.succeed(
          DiscussionActionConfirmed.Rejected(
            DiscussionAction.SetRoomSlot(TopicId(0L), None, None),
          ),
        )
      else
        for
          // 0-99: 0-44 = move to slot (45%), 45-94 = swap (50%), 95-99 = unschedule (5%)
          actionType <- Random.nextIntBounded(100)
          action <- actionType match
            case n if n < 45 => randomMoveToSlotAction(currentState)
            case n if n < 95 => randomSwapAction(currentState)
            case _ => randomUnscheduleAction(currentState)
          result <- applyAction(action)
        yield result
    yield result

  /** Pick a random topic and move it to an available slot */
  private def randomMoveToSlotAction(currentState: DiscussionState): Task[DiscussionAction] =
    val topics = currentState.data.values.toList
    val availableSlots = findAvailableSlots(currentState)
    for
      topicIdx <- Random.nextIntBounded(topics.size.max(1))
      slotIdx <- Random.nextIntBounded(availableSlots.size.max(1))
    yield
      val topic = topics.lift(topicIdx)
      val slot = availableSlots.lift(slotIdx)
      (topic, slot) match
        case (Some(t), Some(s)) =>
          DiscussionAction.SetRoomSlot(t.id, t.roomSlot, Some(s))
        case (Some(t), None) =>
          DiscussionAction.SetRoomSlot(t.id, t.roomSlot, None) // No slots available, unschedule instead
        case _ =>
          DiscussionAction.SetRoomSlot(TopicId(0L), None, None) // Will be rejected

  /** Pick a random scheduled topic and unschedule it */
  private def randomUnscheduleAction(currentState: DiscussionState): Task[DiscussionAction] =
    val scheduledTopics = currentState.data.values.filter(_.roomSlot.isDefined).toList
    for
      idx <- Random.nextIntBounded(scheduledTopics.size.max(1))
    yield scheduledTopics.lift(idx) match
      case Some(topic) =>
        DiscussionAction.SetRoomSlot(topic.id, topic.roomSlot, None)
      case None =>
        DiscussionAction.SetRoomSlot(TopicId(0L), None, None) // Will be rejected

  /** Pick two scheduled topics and swap their slots */
  private def randomSwapAction(currentState: DiscussionState): Task[DiscussionAction] =
    val scheduledTopics = currentState.data.values.filter(_.roomSlot.isDefined).toList
    for
      idx1 <- Random.nextIntBounded(scheduledTopics.size.max(1))
      idx2 <- Random.nextIntBounded(scheduledTopics.size.max(1))
    yield
      val topic1 = scheduledTopics.lift(idx1)
      val topic2 = scheduledTopics.lift(idx2).filter(_ != topic1.orNull)
      (topic1, topic2) match
        case (Some(t1), Some(t2)) if t1.roomSlot.isDefined && t2.roomSlot.isDefined =>
          DiscussionAction.SwapTopics(t1.id, t1.roomSlot.get, t2.id, t2.roomSlot.get)
        case _ => DiscussionAction.SetRoomSlot(TopicId(0L), None, None) // Will be rejected

  /** Find all available (unoccupied) room slots */
  private def findAvailableSlots(currentState: DiscussionState): List[RoomSlot] =
    currentState.slots.flatMap { daySlot =>
      daySlot.slots.flatMap { timeSlotForAllRooms =>
        timeSlotForAllRooms.rooms.flatMap { room =>
          val slot = RoomSlot(room, timeSlotForAllRooms.time)
          val occupied = currentState.data.values.exists(_.roomSlot.contains(slot))
          if occupied then None else Some(slot)
        }
      }
    }

  private def randomExistingTopicId(currentState: DiscussionState): Task[Option[TopicId]] =
    val keys = currentState.data.keys.toList
    if keys.isEmpty then
      ZIO.succeed(None)
    else
      for
        idx <- Random.nextIntBounded(keys.size)
      yield Some(keys(idx))

  private def applyActionWithActor(
    action: DiscussionAction,
    actor: String
  ): Task[DiscussionActionConfirmed] =
    action match
      case add @ DiscussionAction.Add(topic, facilitator) =>
        for
          _            <- ensureUserExists(actor)
          currentState <- state.get
          atCapacity = currentState.data.size >= MaxTopicCount
          result <- if atCapacity then
            ZIO.succeed(DiscussionActionConfirmed.Rejected(add))
          else
            for
              discussion <- createDiscussion(topic, facilitator, None)
              _          <- persistEvent("Add", discussion.id.unwrap, action.toJson, actor)
              _          <- persistDiscussion(discussion)
              _          <- persistDiscussionVotes(discussion)
              _          <- state.update(_.apply(discussion))
            yield DiscussionActionConfirmed.AddResult(discussion)
        yield result

      case addWithRoom @ DiscussionAction.AddWithRoomSlot(topic, facilitator, roomSlot) =>
        for
          _            <- ensureUserExists(actor)
          currentState <- state.get
          atCapacity = currentState.data.size >= MaxTopicCount
          slotOccupied = currentState.data.values.exists(_.roomSlot.contains(roomSlot))
          result <- if atCapacity || slotOccupied then
            ZIO.succeed(DiscussionActionConfirmed.Rejected(addWithRoom))
          else
            for
              discussion <- createDiscussion(topic, facilitator, Some(roomSlot))
              _          <- persistEvent("AddWithRoomSlot", discussion.id.unwrap, action.toJson, actor)
              _          <- persistDiscussion(discussion)
              _          <- persistDiscussionVotes(discussion)
              _          <- state.update(_.apply(discussion))
            yield DiscussionActionConfirmed.AddResult(discussion)
        yield result

      case swap @ DiscussionAction.SwapTopics(topic1, expectedSlot1, topic2, expectedSlot2) =>
        for
          _            <- ensureUserExists(actor)
          currentState <- state.get
          actualSlot1 = currentState.data.get(topic1).flatMap(_.roomSlot)
          actualSlot2 = currentState.data.get(topic2).flatMap(_.roomSlot)
          slotsMatch = actualSlot1.contains(expectedSlot1) && actualSlot2.contains(expectedSlot2)
          result <- if !slotsMatch then
            ZIO.succeed(DiscussionActionConfirmed.Rejected(swap))
          else
            val confirmed = DiscussionActionConfirmed.fromDiscussionAction(swap)
            for
              _ <- persistEvent("SwapTopics", topic1.unwrap, action.toJson, actor)
              _ <- updateDiscussionRoomSlot(topic1, Some(expectedSlot2))
              _ <- updateDiscussionRoomSlot(topic2, Some(expectedSlot1))
              _ <- state.update(_.apply(confirmed))
            yield confirmed
        yield result

      case setSlot @ DiscussionAction.SetRoomSlot(topicId,
                                                  expectedCurrentRoomSlot,
                                                  newRoomSlot,
          ) =>
        for
          _            <- ensureUserExists(actor)
          currentState <- state.get
          actualCurrentRoomSlot = currentState.data.get(topicId).flatMap(_.roomSlot)
          currentMatches = actualCurrentRoomSlot == expectedCurrentRoomSlot
          targetOccupied = newRoomSlot.exists(slot =>
            currentState.data.values.exists(d =>
              d.id != topicId && d.roomSlot.contains(slot),
            ),
          )
          result <- if !currentMatches || targetOccupied then
            ZIO.succeed(DiscussionActionConfirmed.Rejected(setSlot))
          else
            val confirmed = DiscussionActionConfirmed.fromDiscussionAction(
              setSlot,
            )
            for
              _ <- persistEvent("SetRoomSlot", topicId.unwrap, action.toJson, actor)
              _ <- updateDiscussionRoomSlot(topicId, newRoomSlot)
              _ <- state.update(_.apply(confirmed))
            yield confirmed
        yield result

      case DiscussionAction.Vote(topicId, feedback) =>
        for
          _ <- ensureUserExists(actor)
          _ <- persistEvent("Vote", topicId.unwrap, action.toJson, actor)
          voteResult <- upsertVoteIfDiscussionExists(topicId, feedback)
          confirmed = DiscussionActionConfirmed.Vote(
            topicId,
            feedback.copy(
              firstVotedAtEpochMs = voteResult.map(_.firstVotedAt.toInstant.toEpochMilli),
            ),
          )
          _ <- state.update(_.apply(confirmed))
        yield confirmed

      case DiscussionAction.ResetUser(person) =>
        for
          _ <- ensureUserExists(actor)
          currentState <- state.get
          // Find topics to delete (created by this person)
          deletedTopicIds = currentState.data.values
            .filter(_.facilitator == person)
            .map(_.id)
            .toList
          // Find topics where this person has voted (excluding ones being deleted)
          clearedVoteTopicIds = currentState.data.values
            .filter(d => !deletedTopicIds.contains(d.id) && d.interestedParties.exists(_.voter == person))
            .map(_.id)
            .toList
          confirmed = DiscussionActionConfirmed.ResetUser(person, deletedTopicIds, clearedVoteTopicIds)
          _ <- persistEvent("ResetUser", 0L, action.toJson, actor)
          // Soft delete the topics from DB
          _ <- ZIO.foreachDiscard(deletedTopicIds) { topicId =>
            discussionRepo.softDelete(topicId.unwrap)
          }
          // Clear votes from remaining topics
          _ <- ZIO.foreachDiscard(clearedVoteTopicIds) { topicId =>
            topicVoteRepo.deleteVote(topicId.unwrap, person.unwrap)
          }
          _ <- state.update(_.apply(confirmed))
        yield confirmed

      case DiscussionAction.Rename(topicId, newTopic) =>
        for
          _ <- ensureUserExists(actor)
          confirmed = DiscussionActionConfirmed.fromDiscussionAction(action)
          _ <- persistEvent("Rename", topicId.unwrap, action.toJson, actor)
          _ <- updateDiscussionTopic(topicId, newTopic)
          _ <- state.update(_.apply(confirmed))
        yield confirmed

      case DiscussionAction.Delete(topicId) =>
        for
          _ <- ensureUserExists(actor)
          confirmed = DiscussionActionConfirmed.fromDiscussionAction(action)
          _ <- persistEvent("Delete", topicId.unwrap, action.toJson, actor)
          _ <- discussionRepo.softDelete(topicId.unwrap)
          _ <- state.update(_.apply(confirmed))
        yield confirmed

  /** Ensure user exists in DB (auto-create if needed for system/anonymous actions) */
  private def ensureUserExists(username: String): Task[Unit] =
    if username == "system" then
      userRepo.upsert(username, Some(username)).unit
    else
      gitHubProfileService.ensureUserWithDisplayName(username).unit

  private def createDiscussion(
    topic: Topic,
    facilitator: Person,
    roomSlot: Option[RoomSlot]
  ): Task[Discussion] =
    for
      randomIcon <- glyphiconService.getRandomIcon
      randomId   <- Random.nextLong
      userRow    <- userRepo.findByUsername(facilitator.unwrap)
      createdAtEpochMs = java.lang.System.currentTimeMillis()
      displayName = userRow.flatMap(_.displayName)
    yield Discussion(
      topic,
      facilitator,
      Set(Feedback(facilitator, VotePosition.Interested, Some(createdAtEpochMs))),
      TopicId(randomId),
      randomIcon,
      roomSlot,
      displayName,
      None,
      createdAtEpochMs
    )

  private def persistEvent(
    eventType: String,
    topicId: Long,
    payload: String,
    actor: String
  ): Task[Unit] =
    eventRepo.append(eventType, topicId, payload, actor).unit

  private def persistDiscussion(discussion: Discussion): Task[Unit] =
    val row = DiscussionRow.create(
      id = discussion.id.unwrap,
      topic = discussion.topic.unwrap,
      facilitator = discussion.facilitator.unwrap,
      glyphicon = discussion.glyphicon.name,
      roomSlot = discussion.roomSlot.map(_.toJson)
    )
    discussionRepo.insert(row)

  private def persistDiscussionVotes(discussion: Discussion): Task[Unit] =
    ZIO.foreachDiscard(discussion.interestedParties) { feedback =>
      topicVoteRepo.upsertVote(
        discussion.id.unwrap,
        feedback.voter.unwrap,
        feedback.position,
      ).unit
    }

  private def updateDiscussionRoomSlot(topicId: TopicId, roomSlot: Option[RoomSlot]): Task[Unit] =
    for
      existing <- discussionRepo.findById(topicId.unwrap)
      _ <- existing match
        case Some(row) =>
          discussionRepo.update(row.copy(roomSlot = roomSlot.map(_.toJson)))
        case None =>
          ZIO.unit
    yield ()

  private def upsertVoteIfDiscussionExists(
    topicId: TopicId,
    feedback: Feedback
  ): Task[Option[TopicVoteRow]] =
    for
      existing <- discussionRepo.findById(topicId.unwrap)
      persisted <- existing match
        case Some(_) =>
          topicVoteRepo.upsertVote(
            topicId.unwrap,
            feedback.voter.unwrap,
            feedback.position,
          ).map(Some(_))
        case None =>
          ZIO.none
    yield persisted

  private def updateDiscussionTopic(topicId: TopicId, newTopic: Topic): Task[Unit] =
    for
      existing <- discussionRepo.findById(topicId.unwrap)
      _ <- existing match
        case Some(row) =>
          discussionRepo.update(row.copy(topic = newTopic.unwrap))
        case None =>
          ZIO.unit
    yield ()

  /** Extract actor (GitHub username) from the action */
  private def extractActor(action: DiscussionAction): String =
    action match
      case DiscussionAction.Add(_, facilitator)              => facilitator.unwrap
      case DiscussionAction.AddWithRoomSlot(_, facilitator, _) => facilitator.unwrap
      case DiscussionAction.Delete(_)                        => "system" // TODO: track who deleted
      case DiscussionAction.Vote(_, feedback)                => feedback.voter.unwrap
      case DiscussionAction.ResetUser(person)                => person.unwrap
      case DiscussionAction.Rename(_, _)                     => "system" // TODO: track who renamed
      case DiscussionAction.SetRoomSlot(_, _, _)             => "system" // TODO: track who scheduled
      case DiscussionAction.SwapTopics(_, _, _, _)           => "system"

object PersistentDiscussionStore:
  /** Load initial state from database */
  def loadInitialState(
    discussionRepo: DiscussionRepository,
    topicVoteRepo: TopicVoteRepository,
    gitHubProfileService: GitHubProfileService,
  ): Task[DiscussionState] =
    for
      rows <- discussionRepo.findAllActive
      voteRows <- topicVoteRepo.findAllForActiveDiscussions
      votesByTopic = voteRows.groupMap(_.topicId) { voteRow =>
        VotePosition.values
          .find(_.toString == voteRow.position)
          .map(position =>
            Feedback(
              Person(voteRow.githubUsername),
              position,
              Some(voteRow.firstVotedAt.toInstant.toEpochMilli),
            ),
          )
      }.view.mapValues(_.flatten.toSet).toMap
      // Get all unique facilitators
      facilitators = rows.map(_.facilitator).distinct
      // Fetch user info for all facilitators
      userRows <- ZIO.foreach(facilitators)(username =>
        gitHubProfileService.ensureUserWithDisplayName(username).either.map(_.toOption),
      )
      userMap = userRows.flatten.map(u => u.githubUsername -> u.displayName.orElse(Some(u.githubUsername))).toMap
      discussions = rows.flatMap { row =>
        for
          topic <- Topic.make(row.topic).toOption
          facilitator = Person(row.facilitator)
          glyphicon = Glyphicon(row.glyphicon)
          roomSlot = row.roomSlot.flatMap(_.fromJson[RoomSlot].toOption)
          parties = votesByTopic.getOrElse(row.id, Set.empty)
        yield Discussion(
          topic,
          facilitator,
          parties,
          TopicId(row.id),
          glyphicon,
          roomSlot,
          userMap.get(row.facilitator).flatten,
          row.slackPermalink,
          row.createdAt.toInstant.toEpochMilli
        )
      }
    yield DiscussionState(
      DiscussionState.timeSlotExamples,
      discussions.map(d => d.id -> d).toMap
    )

  val layer: ZLayer[
    EventRepository & DiscussionRepository & TopicVoteRepository & UserRepository & GitHubProfileService & GlyphiconService,
    Throwable,
    DiscussionStore
  ] =
    ZLayer.fromZIO:
      for
        eventRepo      <- ZIO.service[EventRepository]
        discussionRepo <- ZIO.service[DiscussionRepository]
        topicVoteRepo  <- ZIO.service[TopicVoteRepository]
        userRepo       <- ZIO.service[UserRepository]
        gitHubProfileService <- ZIO.service[GitHubProfileService]
        glyphiconSvc   <- ZIO.service[GlyphiconService]
        initialState   <- loadInitialState(discussionRepo, topicVoteRepo, gitHubProfileService)
        stateRef       <- Ref.make(initialState)
        _              <- ZIO.logInfo(s"Loaded ${initialState.data.size} discussions from database")
      yield PersistentDiscussionStore(
        eventRepo,
        discussionRepo,
        topicVoteRepo,
        userRepo,
        gitHubProfileService,
        glyphiconSvc,
        stateRef
      )
