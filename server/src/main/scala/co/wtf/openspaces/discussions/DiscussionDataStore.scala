package co.wtf.openspaces.discussions

import co.wtf.openspaces.{GlyphiconService, Person, RoomSlot, Topic, TopicId}
import zio.*
import zio.direct.*

class DiscussionDataStore(
  discussionDatabase: Ref[DiscussionState],
  glyphiconService: GlyphiconService) extends DiscussionStore:
  
  /** Maximum number of topics allowed. Protects against runaway topic creation. */
  private val MaxTopicCount = 200

  def snapshot = discussionDatabase.get

  def applyConfirmed(action: DiscussionActionConfirmed): UIO[Unit] =
    discussionDatabase.update(_.apply(action))

  def applyAction(
    discussionAction: DiscussionAction,
  ): UIO[DiscussionActionConfirmed] =
    discussionAction match
      case add @ DiscussionAction.Add(topic, facilitator) =>
        defer:
          val currentState = discussionDatabase.get.run
          if currentState.data.size >= MaxTopicCount then
            DiscussionActionConfirmed.Rejected(add)
          else
            val discussion = createDiscussion(topic, facilitator, None).run
            discussionDatabase.updateAndGet(s => s(discussion)).run
            DiscussionActionConfirmed.AddResult(discussion)
            
      case addWithRoom @ DiscussionAction.AddWithRoomSlot(topic,
                                                          facilitator,
                                                          roomSlot,
          ) =>
        defer:
          val currentState = discussionDatabase.get.run
          val atCapacity = currentState.data.size >= MaxTopicCount
          val slotOccupied = currentState.data.values.exists(
            _.roomSlot.contains(roomSlot),
          )
          if atCapacity || slotOccupied then 
            DiscussionActionConfirmed.Rejected(addWithRoom)
          else
            val discussion =
              createDiscussion(topic,
                               facilitator,
                               Some(roomSlot),
              ).run
            discussionDatabase.updateAndGet(s => s(discussion)).run
            DiscussionActionConfirmed.AddResult(discussion)

      case swap @ DiscussionAction.SwapTopics(topic1,
                                              expectedSlot1,
                                              topic2,
                                              expectedSlot2,
          ) =>
        defer:
          val currentState = discussionDatabase.get.run
          val actualSlot1 =
            currentState.data.get(topic1).flatMap(_.roomSlot)
          val actualSlot2 =
            currentState.data.get(topic2).flatMap(_.roomSlot)

          // Validate that the room slots haven't changed since the user saw them
          val slotsMatch =
            actualSlot1.contains(expectedSlot1) &&
              actualSlot2.contains(expectedSlot2)

          if (slotsMatch)
            val confirmedAction =
              DiscussionActionConfirmed.fromDiscussionAction(swap)
            discussionDatabase
              .updateAndGet(s => s(confirmedAction))
              .run
            confirmedAction
          else DiscussionActionConfirmed.Rejected(swap)

      case setSlot @ DiscussionAction.SetRoomSlot(topicId,
                                                  expectedCurrentRoomSlot,
                                                  newRoomSlot,
          ) =>
        defer:
          val currentState = discussionDatabase.get.run
          val actualCurrentRoomSlot =
            currentState.data.get(topicId).flatMap(_.roomSlot)
          val currentMatches = actualCurrentRoomSlot == expectedCurrentRoomSlot
          val targetOccupied = newRoomSlot.exists { slot =>
            currentState.data.values.exists { discussion =>
              discussion.id != topicId && discussion.roomSlot.contains(slot)
            }
          }

          if !currentMatches || targetOccupied then
            DiscussionActionConfirmed.Rejected(setSlot)
          else
            val confirmedAction = DiscussionActionConfirmed.fromDiscussionAction(
              setSlot,
            )
            discussionDatabase
              .updateAndGet(s => s(confirmedAction))
              .run
            confirmedAction
      // TODO Case for voting - IF a topic was deleted, the vote should be rejected.

      case other =>
        defer:
          val confirmedAction =
            DiscussionActionConfirmed.fromDiscussionAction(other)
          discussionDatabase.updateAndGet(s => s(confirmedAction)).run
          confirmedAction

  /** Minimum number of topics to maintain during chaos mode.
    * Prevents death spiral where deletions outpace creations.
    */
  private val MinTopicCount = 5

  private def randomExistingTopicId: UIO[Option[TopicId]] =
    defer:
      val data = snapshot.run
      val keys = data.data.keys.toList
      if keys.isEmpty then
        None
      else
        val idx = Random.nextIntBounded(keys.length).run
        Some(keys(idx))

  private def makeAddAction: UIO[DiscussionAction.Add] =
    defer:
      val person = Person("RandomPerson - " + Random.nextIntBounded(20).run)
      DiscussionAction.Add(DiscussionTopics.randomTopic.run, person)

  def randomDiscussionAction =
    defer:
      val actionIdx = Random.nextIntBounded(10).run
      val currentState = snapshot.run
      val topicCount = currentState.data.size
      val noCurrentItems = topicCount == 0
      val belowMinimum = topicCount < MinTopicCount

      val action: DiscussionAction =
        if (noCurrentItems || belowMinimum)
          // Always add when below minimum to prevent death spiral
          makeAddAction.run
        else
          actionIdx match {
            case 0 =>
              makeAddAction.run
            case 1 =>
              randomExistingTopicId.run match
                case Some(id) => DiscussionAction.Delete(id)
                case None => makeAddAction.run
            case 2 | 3 | 4 =>
              randomExistingTopicId.run match
                case Some(id) =>
                  val person = Person(
                    "RandomPerson - " + Random.nextIntBounded(20).run,
                  )
                  DiscussionAction.Vote(
                    id,
                    Feedback(person, VotePosition.Interested),
                  )
                case None => makeAddAction.run

            case 5 | 6 | 7 =>
              randomExistingTopicId.run match
                case Some(id) =>
                  val person = Person(
                    "RandomPerson - " + Random.nextIntBounded(20).run,
                  )
                  DiscussionAction.Vote(
                    id,
                    Feedback(person, VotePosition.NotInterested),
                  )
                case None => makeAddAction.run
            case 8 =>
              randomExistingTopicId.run match
                case Some(id) =>
                  val newTopic = DiscussionTopics.randomTopic.run
                  DiscussionAction.Rename(id, newTopic)
                case None => makeAddAction.run
            case 9 =>
              randomExistingTopicId.run match
                case None => makeAddAction.run
                case Some(id) =>
                  val state = discussionDatabase.get.run
                  val slots = state.slots
                  // Find an available slot through some truly gory inline logic
                  val availableSlot =
                    slots
                      .flatMap(slot =>
                        slot.slots.flatMap(timeSlotForAllRooms =>
                          timeSlotForAllRooms.rooms
                            .find(room =>
                              !state.data.values.exists(discussion =>
                                discussion.roomSlot.contains(
                                  RoomSlot(room,
                                           timeSlotForAllRooms.time,
                                  ),
                                ),
                              ),
                            )
                            .map(room =>
                              RoomSlot(
                                room,
                                timeSlotForAllRooms.time,
                              ),
                            ),
                        ),
                      )
                      .headOption
                  availableSlot match {
                    case None =>
                      // All slots full - vote instead of deleting to avoid death spiral
                      val person = Person(
                        "RandomPerson - " + Random.nextIntBounded(20).run,
                      )
                      DiscussionAction.Vote(
                        id,
                        Feedback(person, VotePosition.Interested),
                      )
                    case Some(roomSlot) =>
                      DiscussionAction.SetRoomSlot(
                        id,
                        state.data.get(id).flatMap(_.roomSlot),
                        Some(roomSlot),
                      )
                  }
          }
      applyAction(action).run

  /** Random schedule-only action for schedule chaos mode.
    * Heavily biased towards scheduling and swapping (95%), unscheduling rare (5%).
    */
  def randomScheduleAction: Task[DiscussionActionConfirmed] =
    defer:
      val currentState = snapshot.run
      val topics = currentState.data.values.toList
      if topics.isEmpty then
        DiscussionActionConfirmed.Rejected(
          DiscussionAction.SetRoomSlot(TopicId(0L), None, None),
        )
      else
        // 0-99: 0-44 = move to slot (45%), 45-94 = swap (50%), 95-99 = unschedule (5%)
        val actionType = Random.nextIntBounded(100).run
        val action: DiscussionAction = actionType match
          case n if n < 45 =>
            // Move to slot (45%)
            val topicIdx = Random.nextIntBounded(topics.size).run
            val topic = topics(topicIdx)
            findAvailableSlot(currentState) match
              case Some(slot) =>
                DiscussionAction.SetRoomSlot(topic.id, topic.roomSlot, Some(slot))
              case None =>
                DiscussionAction.SetRoomSlot(topic.id, topic.roomSlot, None)
          case n if n < 95 =>
            // Swap (50%)
            val scheduled = topics.filter(_.roomSlot.isDefined)
            if scheduled.size < 2 then
              DiscussionAction.SetRoomSlot(TopicId(0L), None, None)
            else
              val idx1 = Random.nextIntBounded(scheduled.size).run
              val idx2 = Random.nextIntBounded(scheduled.size).run
              val t1 = scheduled(idx1)
              val t2 = scheduled(idx2)
              if t1 != t2 && t1.roomSlot.isDefined && t2.roomSlot.isDefined then
                DiscussionAction.SwapTopics(t1.id, t1.roomSlot.get, t2.id, t2.roomSlot.get)
              else
                DiscussionAction.SetRoomSlot(TopicId(0L), None, None)
          case _ =>
            // Unschedule (5%)
            val scheduled = topics.filter(_.roomSlot.isDefined)
            if scheduled.isEmpty then
              DiscussionAction.SetRoomSlot(TopicId(0L), None, None)
            else
              val idx = Random.nextIntBounded(scheduled.size).run
              val topic = scheduled(idx)
              DiscussionAction.SetRoomSlot(topic.id, topic.roomSlot, None)
        applyAction(action).run

  private def findAvailableSlot(state: DiscussionState): Option[RoomSlot] =
    state.slots
      .flatMap(daySlot =>
        daySlot.slots.flatMap(timeSlotForAllRooms =>
          timeSlotForAllRooms.rooms.flatMap { room =>
            val slot = RoomSlot(room, timeSlotForAllRooms.time)
            if state.data.values.exists(_.roomSlot.contains(slot)) then None
            else Some(slot)
          }
        )
      )
      .headOption

  private def createDiscussion(
    topic: Topic,
    facilitator: Person,
    roomSlot: Option[RoomSlot],
  ): UIO[Discussion] =
    for {
      randomIcon <- glyphiconService.getRandomIcon
      randomId   <- Random.nextLong
      createdAtEpochMs = java.lang.System.currentTimeMillis()
    } yield Discussion(
      topic,
      facilitator,
      Set(Feedback(facilitator, VotePosition.Interested)),
      TopicId(randomId),
      randomIcon,
      roomSlot,
      None,
      None,
      createdAtEpochMs,
    )

object DiscussionDataStore:
  def layer(
    useSampleData: Boolean,
  ) =
    ZLayer.fromZIO:
      defer:
        val initialState =
          if useSampleData then DiscussionState.exampleWithDiscussions
          else DiscussionState.example
        DiscussionDataStore(
          Ref.make(initialState).run,
          ZIO.service[GlyphiconService].run,
        )
        //
