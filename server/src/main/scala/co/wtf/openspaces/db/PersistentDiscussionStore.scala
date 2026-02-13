package co.wtf.openspaces.db

import co.wtf.openspaces.*
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
  userRepo: UserRepository,
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
  private val randomUserPool: List[Person] = List(
    Person("kitlangton"),
    Person("jamesward"),
    Person("BruceEckel"),
  )

  private def randomPerson: Task[Person] =
    for
      idx <- Random.nextIntBounded(randomUserPool.size)
    yield randomUserPool(idx)

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
          case Some(roomSlot) => DiscussionAction.UpdateRoomSlot(topicId, roomSlot)
          // No slots available - vote instead of deleting to avoid death spiral
          case None => DiscussionAction.Vote(topicId, Feedback(person, VotePosition.Interested))

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

      case move @ DiscussionAction.MoveTopic(topicId, targetRoomSlot) =>
        for
          _            <- ensureUserExists(actor)
          currentState <- state.get
          targetOccupied = currentState.data.values.exists(d =>
            d.id != topicId && d.roomSlot.contains(targetRoomSlot)
          )
          result <- if targetOccupied then
            ZIO.succeed(DiscussionActionConfirmed.Rejected(move))
          else
            val confirmed = DiscussionActionConfirmed.fromDiscussionAction(move)
            for
              _ <- persistEvent("MoveTopic", topicId.unwrap, action.toJson, actor)
              _ <- updateDiscussionRoomSlot(topicId, Some(targetRoomSlot))
              _ <- state.update(_.apply(confirmed))
            yield confirmed
        yield result

      case DiscussionAction.Vote(topicId, feedback) =>
        for
          _ <- ensureUserExists(actor)
          confirmed = DiscussionActionConfirmed.fromDiscussionAction(action)
          _ <- persistEvent("Vote", topicId.unwrap, action.toJson, actor)
          // Upsert: remove any existing vote from this voter, then add the new vote
          _ <- updateDiscussionParties(topicId, parties => 
            parties.filterNot(_.voter == feedback.voter) + feedback)
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
            updateDiscussionParties(topicId, parties => parties.filterNot(_.voter == person))
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

      case DiscussionAction.UpdateRoomSlot(topicId, roomSlot) =>
        for
          _ <- ensureUserExists(actor)
          confirmed = DiscussionActionConfirmed.fromDiscussionAction(action)
          _ <- persistEvent("UpdateRoomSlot", topicId.unwrap, action.toJson, actor)
          _ <- updateDiscussionRoomSlot(topicId, Some(roomSlot))
          _ <- state.update(_.apply(confirmed))
        yield confirmed

      case DiscussionAction.Unschedule(topicId) =>
        for
          _ <- ensureUserExists(actor)
          confirmed = DiscussionActionConfirmed.fromDiscussionAction(action)
          _ <- persistEvent("Unschedule", topicId.unwrap, action.toJson, actor)
          _ <- updateDiscussionRoomSlot(topicId, None)
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
    userRepo.upsert(username, None).unit

  private def createDiscussion(
    topic: Topic,
    facilitator: Person,
    roomSlot: Option[RoomSlot]
  ): Task[Discussion] =
    for
      randomIcon <- glyphiconService.getRandomIcon
      randomId   <- Random.nextLong
      userRow    <- userRepo.findByUsername(facilitator.unwrap)
      displayName = userRow.flatMap(_.displayName)
    yield Discussion(
      topic,
      facilitator,
      Set(Feedback(facilitator, VotePosition.Interested)),
      TopicId(randomId),
      randomIcon,
      roomSlot,
      displayName,
      None
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
      roomSlot = discussion.roomSlot.map(_.toJson),
      interestedParties = discussion.interestedParties.toJson
    )
    discussionRepo.insert(row)

  private def updateDiscussionRoomSlot(topicId: TopicId, roomSlot: Option[RoomSlot]): Task[Unit] =
    for
      existing <- discussionRepo.findById(topicId.unwrap)
      _ <- existing match
        case Some(row) =>
          discussionRepo.update(row.copy(roomSlot = roomSlot.map(_.toJson)))
        case None =>
          ZIO.unit
    yield ()

  private def updateDiscussionParties(
    topicId: TopicId,
    f: Set[Feedback] => Set[Feedback]
  ): Task[Unit] =
    for
      existing <- discussionRepo.findById(topicId.unwrap)
      _ <- existing match
        case Some(row) =>
          val parties = row.interestedParties.fromJson[Set[Feedback]].getOrElse(Set.empty)
          val updated = f(parties)
          discussionRepo.update(row.copy(interestedParties = updated.toJson))
        case None =>
          ZIO.unit
    yield ()

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
      case DiscussionAction.UpdateRoomSlot(_, _)             => "system" // TODO: track who scheduled
      case DiscussionAction.Unschedule(_)                    => "system"
      case DiscussionAction.MoveTopic(_, _)                  => "system"
      case DiscussionAction.SwapTopics(_, _, _, _)           => "system"

object PersistentDiscussionStore:
  /** Load initial state from database */
  def loadInitialState(discussionRepo: DiscussionRepository, userRepo: UserRepository): Task[DiscussionState] =
    for
      rows <- discussionRepo.findAllActive
      // Get all unique facilitators
      facilitators = rows.map(_.facilitator).distinct
      // Fetch user info for all facilitators
      userRows <- ZIO.foreach(facilitators)(username => userRepo.findByUsername(username))
      userMap = userRows.flatten.map(u => u.githubUsername -> u.displayName).toMap
      discussions = rows.flatMap { row =>
        for
          topic <- Topic.make(row.topic).toOption
          facilitator = Person(row.facilitator)
          glyphicon = Glyphicon(row.glyphicon)
          roomSlot = row.roomSlot.flatMap(_.fromJson[RoomSlot].toOption)
          parties = row.interestedParties.fromJson[Set[Feedback]].getOrElse(Set.empty)
        yield Discussion(
          topic,
          facilitator,
          parties,
          TopicId(row.id),
          glyphicon,
          roomSlot,
          userMap.get(row.facilitator).flatten,
          row.slackPermalink
        )
      }
    yield DiscussionState(
      DiscussionState.timeSlotExamples,
      discussions.map(d => d.id -> d).toMap
    )

  val layer: ZLayer[
    EventRepository & DiscussionRepository & UserRepository & GlyphiconService,
    Throwable,
    DiscussionStore
  ] =
    ZLayer.fromZIO:
      for
        eventRepo      <- ZIO.service[EventRepository]
        discussionRepo <- ZIO.service[DiscussionRepository]
        userRepo       <- ZIO.service[UserRepository]
        glyphiconSvc   <- ZIO.service[GlyphiconService]
        initialState   <- loadInitialState(discussionRepo, userRepo)
        stateRef       <- Ref.make(initialState)
        _              <- ZIO.logInfo(s"Loaded ${initialState.data.size} discussions from database")
      yield PersistentDiscussionStore(
        eventRepo,
        discussionRepo,
        userRepo,
        glyphiconSvc,
        stateRef
      )
