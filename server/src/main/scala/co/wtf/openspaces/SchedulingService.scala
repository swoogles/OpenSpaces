package co.wtf.openspaces

import zio.*
import java.time.LocalDateTime
import java.time.Duration
import neotype.unwrap

/** Auto-scheduler that assigns topics to room slots based on votes and conflict minimization. */
case class SchedulingService(
  discussionService: DiscussionService,
  discussionStore: DiscussionStore
):

  /** Run the scheduling algorithm and apply the resulting actions.
    * Returns a summary of what was changed.
    */
  def runScheduling: Task[SchedulingSummary] =
    for
      now <- Clock.localDateTime
      state <- discussionStore.snapshot
      discussions = state.data.values.toList
      slots = state.slots
      
      // Extract all rooms and time slots
      allRooms = slots.flatMap(_.slots.flatMap(_.rooms)).distinct.sortBy(-_.capacity)
      allTimeSlots = slots.flatMap(_.slots.map(_.time)).distinct
      
      // Compute frozen slots (starting in < 15 minutes)
      frozenSlots = allTimeSlots.filter { ts =>
        Duration.between(now, ts.startTime).toMinutes < 15
      }.toSet
      
      // Run the algorithm
      actions = computeSchedule(discussions, allRooms, allTimeSlots, frozenSlots)
      
      // Apply computed actions
      _ <- ZIO.foreachDiscard(actions) { action =>
        discussionService.applyAndBroadcast(action)
      }
      
      summary = SchedulingSummary(
        scheduled = actions.count {
          case DiscussionAction.SetRoomSlot(_, None, Some(_)) => true
          case _ => false
        },
        moved = actions.count {
          case DiscussionAction.SetRoomSlot(_, Some(_), Some(_)) => true
          case _ => false
        },
        unscheduled = actions.count {
          case DiscussionAction.SetRoomSlot(_, Some(_), None) => true
          case _ => false
        }
      )
      _ <- ZIO.logInfo(s"Scheduling complete: $summary")
    yield summary

  /** Core scheduling algorithm - computes optimal assignments */
  private def computeSchedule(
    discussions: List[Discussion],
    rooms: List[Room],
    timeSlots: List[TimeSlot],
    frozenSlots: Set[TimeSlot]
  ): List[DiscussionAction] =
    
    // Phase 1: Partition topics
    val frozen = discussions.filter(d => 
      d.roomSlot.exists(rs => frozenSlots.contains(rs.timeSlot))
    )
    val movable = discussions.filterNot(frozen.contains)
    
    // Phase 2: Rank topics by votes (desc), then by id (asc for FCFS)
    val ranked = movable.sortBy(d => (-d.votes, d.id.unwrap))
    
    // Phase 3: Compute available slots (unfrozen)
    val availableSlots: List[RoomSlot] = for
      ts <- timeSlots if !frozenSlots.contains(ts)
      room <- rooms.sortBy(-_.capacity)
    yield RoomSlot(room, ts)
    
    // Phase 4: Greedy assignment with conflict minimization
    val assignments = assignTopics(ranked, availableSlots)
    
    // Phase 5: Right-size rooms within each time slot
    val rightSized = rightSizeRooms(assignments, rooms, discussions)
    
    // Phase 6: Compute diff (actions needed)
    computeActions(discussions, rightSized, frozen.map(_.id).toSet)

  /** Greedy assignment: process topics by vote count, assign to slot with fewest conflicts */
  private def assignTopics(
    ranked: List[Discussion],
    availableSlots: List[RoomSlot]
  ): Map[TopicId, RoomSlot] =
    var assignments: Map[TopicId, RoomSlot] = Map.empty
    var usedSlots: Set[RoomSlot] = Set.empty
    var voterSchedule: Map[Person, Set[TimeSlot]] = Map.empty

    for topic <- ranked do
      val candidates = availableSlots.filterNot(usedSlots.contains)
      
      if candidates.nonEmpty then
        // Find interested voters for this topic
        val interestedVoters = topic.interestedParties
          .filter(_.position == VotePosition.Interested)
          .map(_.voter)
        
        val bestSlot = candidates.minBy { slot =>
          // Count conflicts: voters already booked in this time slot
          val newConflicts = interestedVoters.count { voter =>
            voterSchedule.getOrElse(voter, Set.empty).contains(slot.timeSlot)
          }
          // Penalty if room is too small
          val roomSizePenalty = if slot.room.capacity < topic.votes then 1000 else 0
          // Prefer bigger rooms when tied (negative capacity for minBy)
          (newConflicts, roomSizePenalty, -slot.room.capacity)
        }
        
        assignments += (topic.id -> bestSlot)
        usedSlots += bestSlot
        
        // Update voter schedule
        for voter <- interestedVoters do
          voterSchedule = voterSchedule.updated(
            voter,
            voterSchedule.getOrElse(voter, Set.empty) + bestSlot.timeSlot
          )
    
    assignments

  /** Ensure within each time slot, the most popular topics get the biggest rooms */
  private def rightSizeRooms(
    assignments: Map[TopicId, RoomSlot],
    rooms: List[Room],
    discussions: List[Discussion]
  ): Map[TopicId, RoomSlot] =
    val discussionMap = discussions.map(d => d.id -> d).toMap
    
    assignments
      .groupBy(_._2.timeSlot)
      .flatMap { case (timeSlot, topicsInSlot) =>
        // Sort topics by votes (descending)
        val sortedTopics = topicsInSlot.toList.sortBy { case (id, _) =>
          -discussionMap.get(id).map(_.votes).getOrElse(0)
        }
        // Sort rooms by capacity (descending)
        val sortedRooms = rooms.sortBy(-_.capacity)
        
        // Zip: most popular topic gets biggest room
        sortedTopics.zip(sortedRooms).map { case ((topicId, _), room) =>
          topicId -> RoomSlot(room, timeSlot)
        }
      }
      .toMap

  /** Compute the actions needed to transition from current state to target */
  private def computeActions(
    discussions: List[Discussion],
    target: Map[TopicId, RoomSlot],
    frozenIds: Set[TopicId]
  ): List[DiscussionAction] =
    val actions = scala.collection.mutable.ListBuffer[DiscussionAction]()
    
    for d <- discussions if !frozenIds.contains(d.id) do
      val currentSlot = d.roomSlot
      val targetSlot = target.get(d.id)
      
      (currentSlot, targetSlot) match
        case (None, Some(slot)) =>
          // Was unscheduled, now scheduled
          actions += DiscussionAction.SetRoomSlot(d.id, None, Some(slot))
        
        case (Some(old), Some(newSlot)) if old != newSlot =>
          // Moved to different slot
          actions += DiscussionAction.SetRoomSlot(d.id, Some(old), Some(newSlot))
        
        case (Some(_), None) =>
          // Was scheduled, now unscheduled (bumped by higher-voted topics)
          actions += DiscussionAction.SetRoomSlot(d.id, currentSlot, None)
        
        case _ =>
          // No change needed
          ()
    
    actions.toList

case class SchedulingSummary(
  scheduled: Int,
  moved: Int,
  unscheduled: Int
):
  override def toString: String =
    s"scheduled=$scheduled, moved=$moved, unscheduled=$unscheduled"

object SchedulingService:
  val layer: ZLayer[DiscussionService & DiscussionStore, Nothing, SchedulingService] =
    ZLayer.fromZIO:
      for
        discussionService <- ZIO.service[DiscussionService]
        discussionStore <- ZIO.service[DiscussionStore]
      yield SchedulingService(discussionService, discussionStore)
