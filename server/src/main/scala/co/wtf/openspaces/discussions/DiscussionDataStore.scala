package co.wtf.openspaces.discussions

import co.wtf.openspaces.{GlyphiconService, Person, RoomSlot, Topic, TopicId, UserOps}
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

      case swap @ DiscussionAction.SwapTopics(topic1, expectedSlot1, topic2, expectedSlot2, _) =>
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

      case setSlot @ DiscussionAction.SetRoomSlot(topicId, expectedCurrentRoomSlot, newRoomSlot, _) =>
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
      case setLocked @ DiscussionAction.SetLockedTimeslot(topicId, expectedCurrentLockedTimeslot, newLockedTimeslot, _) =>
        defer:
          val currentState = discussionDatabase.get.run
          val existingTopic = currentState.data.get(topicId)
          val actualCurrentLockedTimeslot =
            existingTopic.map(_.lockedTimeslot)
          val currentMatches = actualCurrentLockedTimeslot.contains(
            expectedCurrentLockedTimeslot,
          )
          val lockWithoutSlot =
            newLockedTimeslot && existingTopic.flatMap(_.roomSlot).isEmpty

          if !currentMatches || lockWithoutSlot then
            DiscussionActionConfirmed.Rejected(setLocked)
          else
            val confirmedAction =
              DiscussionActionConfirmed.fromDiscussionAction(setLocked)
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

  def reloadFromDatabase: Task[DiscussionState] =
    snapshot

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
      lockedTimeslot = false,
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
