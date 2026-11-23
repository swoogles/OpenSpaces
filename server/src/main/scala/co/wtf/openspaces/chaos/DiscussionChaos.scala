package co.wtf.openspaces.chaos

import co.wtf.openspaces.{Person, RandomUsers, RoomSlot, TopicId, UserOps}
import co.wtf.openspaces.db.PersistentDiscussionStore.MaxTopicCount
import co.wtf.openspaces.discussions.{DiscussionAction, DiscussionState, DiscussionTopics, Feedback, VotePosition}
import zio.*

/** Random discussion actions for chaos mode. Generates actions only; applying them
  * goes through the normal SessionService pipeline.
  */
object DiscussionChaos:
  /** Minimum number of topics to maintain during chaos mode. */
  private val MinTopicCount = 5

  /** A weighted random discussion action, used by chaos mode. */
  def randomAction(currentState: DiscussionState): Task[DiscussionAction] =
    val topicCount = currentState.data.size
    if topicCount < MinTopicCount then
      // Always add when below minimum to prevent death spiral
      randomAddAction
    else if topicCount >= MaxTopicCount then
      // At capacity - only do non-add actions
      Random.nextIntBounded(8).flatMap {
        case 0 | 1 | 2 | 3 => randomVoteAction(currentState)
        case 4 | 5 | 6     => randomNotInterestedVoteAction(currentState)
        case _             => randomSlotOrVoteAction(currentState)
      }
    else
      Random.nextIntBounded(10).flatMap {
        case 0             => randomAddAction
        case 1 | 2 | 3 | 4 => randomVoteAction(currentState)
        case 5 | 6 | 7     => randomNotInterestedVoteAction(currentState)
        case _             => randomSlotOrVoteAction(currentState)
      }

  // Pool of real GitHub users for random action generation
  private def randomPerson: UIO[Person] =
    RandomUsers.randomPerson

  private def randomAddAction: Task[DiscussionAction] =
    for
      person <- randomPerson
      topic <- DiscussionTopics.randomTopic
    yield DiscussionAction.Add(topic, person)

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

  private def randomSlotOrVoteAction(currentState: DiscussionState): Task[DiscussionAction] =
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
              person,
            )
          // No slots available - vote instead of deleting to avoid death spiral
          case None => DiscussionAction.Vote(topicId, Feedback(person, VotePosition.Interested))

  /** Random schedule-only action: move to slot, unschedule, or swap. None when there are no topics. */
  def randomScheduleAction(currentState: DiscussionState): Task[Option[DiscussionAction]] =
    if currentState.data.isEmpty then ZIO.none
    else
      Random.nextIntBounded(100).flatMap {
        case n if n < 45 => randomMoveToSlotAction(currentState)
        case n if n < 95 => randomSwapAction(currentState)
        case _           => randomUnscheduleAction(currentState)
      }.map(Some(_))

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
          DiscussionAction.SetRoomSlot(t.id, t.roomSlot, Some(s), UserOps.systemPerson)
        case (Some(t), None) =>
          DiscussionAction.SetRoomSlot(t.id, t.roomSlot, None, UserOps.systemPerson)
        case _ =>
          DiscussionAction.SetRoomSlot(TopicId(0L), None, None, UserOps.systemPerson)

  private def randomUnscheduleAction(currentState: DiscussionState): Task[DiscussionAction] =
    val scheduledTopics = currentState.data.values.filter(_.roomSlot.isDefined).toList
    for
      idx <- Random.nextIntBounded(scheduledTopics.size.max(1))
    yield scheduledTopics.lift(idx) match
      case Some(topic) =>
        DiscussionAction.SetRoomSlot(topic.id, topic.roomSlot, None, UserOps.systemPerson)
      case None =>
        DiscussionAction.SetRoomSlot(TopicId(0L), None, None, UserOps.systemPerson)

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
          DiscussionAction.SwapTopics(t1.id, t1.roomSlot.get, t2.id, t2.roomSlot.get, UserOps.systemPerson)
        case _ => DiscussionAction.SetRoomSlot(TopicId(0L), None, None, UserOps.systemPerson)

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
