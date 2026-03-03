package co.wtf.openspaces.discussions

import co.wtf.openspaces.{Person, Room, RoomSlot, SessionService, TimeSlot, TopicId}
import zio.*
import java.time.{Duration as JavaDuration, Instant, LocalDateTime, ZoneOffset, ZonedDateTime}
import neotype.unwrap

/** Auto-scheduler that assigns topics to room slots based on votes and conflict minimization. */
case class SchedulingService(
  discussionService: SessionService,
  discussionStore: DiscussionStore
):
  private case class SchedulingPlan(
    actions: List[DiscussionAction],
    target: Map[TopicId, RoomSlot],
    availableSlots: List[RoomSlot],
    ranked: List[Discussion],
    immovableIds: Set[TopicId]
  )


  /** Clear all room slot assignments without deleting topics.
    * Returns count of topics that were unscheduled.
    */
  def clearSchedule: Task[Int] =
    for
      state <- discussionStore.snapshot
      discussions = state.data.values.toList
      scheduled = discussions.filter(_.roomSlot.isDefined)
      
      // Generate unschedule actions for all scheduled topics
      actions = scheduled.map { d =>
        DiscussionAction.SetRoomSlot(d.id, d.roomSlot, None)
      }
      
      // Apply all unschedule actions
      _ <- ZIO.foreachDiscard(actions) { action =>
        discussionService.applyAndBroadcast(action)
      }
      
      _ <- ZIO.logInfo(s"Cleared schedule: ${actions.length} topics unscheduled")
    yield actions.length

  /** Run the scheduling algorithm and apply the resulting actions.
    * Returns a summary of what was changed.
    */
  def runScheduling: Task[SchedulingSummary] =
    for
      now <- Clock.localDateTime
      state <- discussionStore.snapshot
      discussions = state.data.values.toList
      slots = state.slots
      lockedExcluded = discussions.count(_.lockedTimeslot)
      
      // Extract all rooms and time slots
      allRooms = slots.flatMap(_.slots.flatMap(_.rooms)).distinct.sortBy(-_.capacity)
      allTimeSlots = slots.flatMap(_.slots.map(_.time)).distinct
      
      // Freeze already-placed topics in slots starting soon, but keep empty rooms
      // available until the slot actually starts.
      frozenRoomSlots = discussions.flatMap(_.roomSlot).filter { roomSlot =>
        SchedulingService.shouldFreezeOccupiedSlot(now, roomSlot.timeSlot)
      }.toSet
      
      // Run the algorithm
      plan = computeSchedule(discussions, allRooms, allTimeSlots, frozenRoomSlots, now)
      actions = plan.actions
      
      // Apply computed actions
      _ <- ZIO.foreachDiscard(actions) { action =>
        discussionService.applyAndBroadcast(action)
      }
      _ <- logUnscheduledDiagnostics(discussions, plan)
      
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
        },
        lockedExcluded = lockedExcluded,
      )
      _ <- ZIO.logInfo(s"Scheduling complete: $summary")
    yield summary

  /** Start a background loop that runs the scheduler every 30 minutes,
    * anchored to 2026-03-03 09:00 MST.
    */
  def startAutomaticScheduling: UIO[Fiber.Runtime[Nothing, Unit]] =
    for
      now <- Clock.instant
      initialDelay = SchedulingService.nextAutoSchedulingDelay(now)
      _ <- ZIO.logInfo(
        s"Automatic scheduling cadence active (anchor=2026-03-03T09:00:00-07:00, initialDelay=$initialDelay, interval=30 minutes)"
      )
      fiber <- (
        ZIO.sleep(initialDelay) *>
          runScheduling
            .tap(summary =>
              ZIO.logInfo(s"Automatic scheduling run complete: $summary")
            )
            .catchAll(error =>
              ZIO.logErrorCause("Automatic scheduling run failed", Cause.fail(error))
            )
            .unit
            .repeat(Schedule.spaced(SchedulingService.automaticSchedulingInterval) && Schedule.forever)
            .unit
      ).forkDaemon
    yield fiber

  /** Core scheduling algorithm - computes optimal assignments */
  private def computeSchedule(
    discussions: List[Discussion],
    rooms: List[Room],
    timeSlots: List[TimeSlot],
    frozenRoomSlots: Set[RoomSlot],
    now: LocalDateTime,
  ): SchedulingPlan =
    
    // Phase 1: Partition topics
    val frozen = discussions.filter(d =>
      d.roomSlot.exists(frozenRoomSlots.contains)
    )
    val locked = discussions.filter(_.lockedTimeslot)
    val immovableIds = (frozen ++ locked).map(_.id).toSet
    val movable = discussions.filterNot(d => immovableIds.contains(d.id))
    
    // Phase 2: Rank topics by votes (desc), then by id (asc for FCFS)
    val ranked = movable.sortBy(d => (-d.votes, d.id.unwrap))
    
    // Phase 3: Compute available slots (unfrozen)
    val lockedSlots = locked.flatMap(_.roomSlot).toSet
    val unavailableSlots = lockedSlots ++ frozenRoomSlots
    val availableSlots: List[RoomSlot] = for
      ts <- timeSlots if SchedulingService.canAssignIntoSlot(now, ts)
      room <- rooms.sortBy(-_.capacity)
      slot = RoomSlot(room, ts)
      if !unavailableSlots.contains(slot)
    yield slot
    
    // Phase 4: Greedy assignment with conflict minimization
    val assignments = assignTopics(ranked, availableSlots)
    
    // Phase 5: Right-size rooms within each time slot
    val rightSized = rightSizeRooms(assignments, rooms, discussions)
    
    // Phase 6: Compute diff (actions needed)
    SchedulingPlan(
      actions = computeActions(discussions, rightSized, immovableIds),
      target = rightSized,
      availableSlots = availableSlots,
      ranked = ranked,
      immovableIds = immovableIds,
    )

  private def logUnscheduledDiagnostics(
    discussions: List[Discussion],
    plan: SchedulingPlan
  ): UIO[Unit] =
    val rankedIndexById = plan.ranked.zipWithIndex.map { case (discussion, idx) =>
      discussion.id -> (idx + 1)
    }.toMap
    val availableSlotCount = plan.availableSlots.size

    val unscheduled = discussions.filter { discussion =>
      val finalSlot =
        if plan.immovableIds.contains(discussion.id) then discussion.roomSlot
        else plan.target.get(discussion.id)
      finalSlot.isEmpty
    }

    ZIO.foreachDiscard(unscheduled) { discussion =>
      val reason =
        if discussion.lockedTimeslot then
          "locked_timeslot"
        else if plan.availableSlots.isEmpty then
          "no_available_future_slots"
        else
          rankedIndexById.get(discussion.id) match
            case Some(rank) if rank > availableSlotCount =>
              s"out_ranked rank=$rank available_slots=$availableSlotCount votes=${discussion.votes}"
            case Some(rank) =>
              s"no_assignment rank=$rank available_slots=$availableSlotCount votes=${discussion.votes}"
            case None =>
              "frozen_or_excluded"

      ZIO.logInfo(
        s"Scheduling left topic unscheduled: id=${discussion.id.unwrap}, topic=${discussion.topic.unwrap}, reason=$reason"
      )
    }.unit

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
  unscheduled: Int,
  lockedExcluded: Int,
):
  override def toString: String =
    s"scheduled=$scheduled, moved=$moved, unscheduled=$unscheduled, lockedExcluded=$lockedExcluded"

object SchedulingService:
  private[openspaces] val automaticSchedulingAnchor: Instant =
    ZonedDateTime
      .of(2026, 3, 2, 9, 30, 0, 0, ZoneOffset.ofHours(-7))
      .toInstant

  private val automaticSchedulingInterval = 30.minutes

  private[openspaces] def shouldFreezeOccupiedSlot(
    now: LocalDateTime,
    timeSlot: TimeSlot,
  ): Boolean =
    JavaDuration.between(now, timeSlot.startTime).toMinutes < 15

  private[openspaces] def canAssignIntoSlot(
    now: LocalDateTime,
    timeSlot: TimeSlot,
  ): Boolean =
    timeSlot.startTime.isAfter(now)

  private[openspaces] def nextAutoSchedulingDelay(now: Instant): zio.Duration =
    if now.isBefore(automaticSchedulingAnchor) then
      zio.Duration.fromJava(JavaDuration.between(now, automaticSchedulingAnchor))
    else
      val elapsedMillis = JavaDuration
        .between(automaticSchedulingAnchor, now)
        .toMillis
      val intervalMillis = automaticSchedulingInterval.toMillis
      val remainderMillis = Math.floorMod(elapsedMillis, intervalMillis)

      if remainderMillis == 0 then zio.Duration.Zero
      else zio.Duration.fromMillis(intervalMillis - remainderMillis)

  val layer: ZLayer[SessionService & DiscussionStore, Nothing, SchedulingService] =
    ZLayer.fromZIO:
      for
        discussionService <- ZIO.service[SessionService]
        discussionStore <- ZIO.service[DiscussionStore]
      yield SchedulingService(discussionService, discussionStore)
