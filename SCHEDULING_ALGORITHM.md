# Scheduling Algorithm

Auto-scheduling topics to room slots based on interest, conflicts, and room capacity.

## Goals

1. **Reduce conflicts** — If many people want topics A and B, schedule them in different time slots
2. **Right-size rooms** — More votes → bigger room
3. **Incremental updates** — New popular topics can earn slots; less popular ones get demoted
4. **Smooth experience** — Avoid jarring reshuffles; respect "inertia" for ties
5. **Late submissions welcome** — The schedule should evolve, not atrophy with early ideas

---

## Core Concepts

### Frozen Slots

A time slot becomes **frozen** when it starts in less than 15 minutes. Frozen slots are excluded from all scheduling operations.

```
if (slot.startTime - now) < 15.minutes then FROZEN
```

This happens per-slot, so the schedule incrementally locks in over the course of the event.

### Conflict Score

A **conflict** occurs when a person has voted "Interested" on two topics scheduled in the same time slot.

```scala
def conflicts(schedule: Map[TimeSlot, List[Discussion]]): Int =
  schedule.values.flatMap { topicsInSlot =>
    for
      t1 <- topicsInSlot
      t2 <- topicsInSlot if t1.id < t2.id  // avoid double-counting
      voter <- t1.interestedVoters intersect t2.interestedVoters
    yield 1
  }.sum
```

**Optimization target:** Minimize total conflicts across all voters and slots.

> Note: "NotInterested" votes do not factor into conflict scoring — they're just polite feedback.

### Tiebreakers

When two topics have equal votes:
1. **Inertia** — If one is already scheduled and the other isn't, the scheduled one keeps its slot
2. **First-come-first-serve** — Lower `topicId` (earlier creation) wins

### Facilitator Conflicts

Facilitators are voters too. Their conflicts are counted in the total, but they receive no special weight. It's expected that facilitators may miss sessions they're interested in.

---

## Inputs

```scala
case class SchedulingInput(
  discussions: List[Discussion],        // All topics (scheduled + unscheduled)
  rooms: List[Room],                     // Sorted by capacity descending
  timeSlots: List[TimeSlot],             // All slots across all days
  frozenSlots: Set[TimeSlot],            // Slots starting in <15 min
  now: LocalDateTime,
)
```

From the database:
- `discussions` — Current state of all topics with votes and current `roomSlot`
- `rooms` — `[King(30), Art Gallery(20), Hawk(15), Dance Hall(10)]`
- `timeSlots` — All available slots for the event
- `frozenSlots` — Computed from `now`

---

## Outputs

A list of `DiscussionAction` events that transition from current state to optimal state:

```scala
sealed trait SchedulingAction
case class Schedule(topicId: TopicId, roomSlot: RoomSlot)   // UpdateRoomSlot
case class Unschedule(topicId: TopicId)                      // Unschedule
case class Move(topicId: TopicId, newRoomSlot: RoomSlot)     // MoveTopic
```

These map directly to existing `DiscussionAction` variants.

---

## Algorithm

### Phase 1: Partition Topics

```scala
val frozen: List[Discussion]      = discussions.filter(d => d.roomSlot.exists(rs => frozenSlots.contains(rs.timeSlot)))
val scheduled: List[Discussion]   = discussions.filter(d => d.roomSlot.isDefined && !frozen.contains(d))
val unscheduled: List[Discussion] = discussions.filter(_.roomSlot.isEmpty)
```

Frozen topics are untouchable. We're optimizing placement for `scheduled ++ unscheduled`.

### Phase 2: Rank Topics

Sort all non-frozen topics by desirability:

```scala
val ranked: List[Discussion] = (scheduled ++ unscheduled)
  .sortBy(d => (-d.votes, d.id.unwrap))  // votes DESC, then topicId ASC (FCFS)
```

### Phase 3: Compute Available Slots

```scala
val availableSlots: List[RoomSlot] = for
  ts <- timeSlots if !frozenSlots.contains(ts)
  room <- rooms.sortBy(-_.capacity)  // bigger rooms first within each slot
yield RoomSlot(room, ts)
```

Total capacity = `availableSlots.size`. If we have more topics than slots, lowest-ranked topics stay unscheduled.

### Phase 4: Assign Topics to Slots (Greedy with Conflict Minimization)

```scala
def assignTopics(
  ranked: List[Discussion],
  availableSlots: List[RoomSlot],
): Map[TopicId, RoomSlot] =
  
  var assignments: Map[TopicId, RoomSlot] = Map.empty
  var usedSlots: Set[RoomSlot] = Set.empty
  var voterSchedule: Map[Person, Set[TimeSlot]] = Map.empty  // tracks where each voter is "booked"

  for topic <- ranked do
    // Find best slot: minimize new conflicts introduced
    val candidates = availableSlots.filterNot(usedSlots.contains)
    
    if candidates.nonEmpty then
      val bestSlot = candidates.minBy { slot =>
        val newConflicts = topic.interestedVoters.count { voter =>
          voterSchedule.getOrElse(voter, Set.empty).contains(slot.timeSlot)
        }
        val roomSizePenalty = if slot.room.capacity < topic.votes then 1000 else 0
        (newConflicts, roomSizePenalty, -slot.room.capacity)  // prefer bigger rooms when tied
      }
      
      assignments += (topic.id -> bestSlot)
      usedSlots += bestSlot
      
      // Update voter schedule
      for voter <- topic.interestedVoters do
        voterSchedule = voterSchedule.updated(
          voter,
          voterSchedule.getOrElse(voter, Set.empty) + bestSlot.timeSlot
        )
  
  assignments
```

**Key behaviors:**
- Topics are processed in vote order (highest first)
- Each topic greedily picks the slot that introduces fewest new conflicts
- Bigger rooms are preferred when conflict scores are equal
- If a topic has more interested voters than room capacity, it strongly prefers a bigger room

### Phase 5: Room Sizing Within Time Slots

After initial assignment, ensure within each time slot the room sizes match vote counts:

```scala
def rightSizeRooms(
  assignments: Map[TopicId, RoomSlot],
  rooms: List[Room],  // sorted by capacity DESC
): Map[TopicId, RoomSlot] =
  
  assignments
    .groupBy(_._2.timeSlot)
    .flatMap { case (timeSlot, topicsInSlot) =>
      val sortedTopics = topicsInSlot.toList.sortBy { case (id, _) => 
        -discussions.find(_.id == id).map(_.votes).getOrElse(0)
      }
      val sortedRooms = rooms.sortBy(-_.capacity)
      
      sortedTopics.zip(sortedRooms).map { case ((topicId, oldSlot), room) =>
        topicId -> RoomSlot(room, timeSlot)
      }
    }
    .toMap
```

### Phase 6: Compute Diff

Compare new assignments to current state, respecting inertia:

```scala
def computeActions(
  current: Map[TopicId, Option[RoomSlot]],
  target: Map[TopicId, RoomSlot],
  discussions: List[Discussion],
): List[DiscussionAction] =
  
  val actions = ListBuffer[DiscussionAction]()
  
  for d <- discussions if !frozen.contains(d) do
    val currentSlot = d.roomSlot
    val targetSlot = target.get(d.id)
    
    (currentSlot, targetSlot) match
      case (None, Some(slot)) => 
        actions += DiscussionAction.UpdateRoomSlot(d.id, slot)
      
      case (Some(old), Some(new_)) if old != new_ =>
        // Only move if there's a vote difference justifying it
        // (Ties keep current position due to ranking stability)
        actions += DiscussionAction.MoveTopic(d.id, new_)
      
      case (Some(_), None) =>
        actions += DiscussionAction.Unschedule(d.id)
      
      case _ => // no change
  
  actions.toList
```

---

## Multi-Day Behavior

### First Run (Many Unscheduled Topics)

Distribute topics across all days of the event. The algorithm naturally spreads things out because:
- Each time slot is treated equally
- Conflict minimization discourages clustering popular topics

### Follow-Up Runs

Only unfrozen slots are reconsidered. As days progress:
- Day 1 slots freeze → locked in
- Day 2+ slots remain fluid until their time approaches

This means early-day schedules stabilize while later days can still evolve based on new topics and votes.

---

## Edge Cases

### More Topics Than Slots

Lowest-ranked topics (by votes, then FCFS) remain unscheduled. They can display in a "Waitlist" or "Unscheduled" section.

### Zero/One Vote Topics

These are scheduled only as a last resort — when higher-voted topics have filled available slots and capacity remains. They sort below everything with 2+ votes.

### New Popular Topic

A late submission that quickly gains votes will:
1. Outrank lower-voted scheduled topics
2. Claim a slot, potentially displacing a less popular topic
3. Displaced topic gets re-evaluated against remaining slots
4. If no slots remain, displaced topic becomes unscheduled

### All Slots Frozen

Algorithm returns empty action list — nothing can change.

---

## Admin Interface

### "Schedule Topics" Button

- **Visibility:** Admin only (facilitator mode or admin flag)
- **Behavior:** Executes full algorithm, emits resulting `DiscussionAction` events
- **Feedback:** Show summary — "Scheduled 5, moved 3, unscheduled 1"

### Future Enhancements (Not MVP)

- [ ] "Dry run" mode — preview changes before applying
- [ ] Pin topics to slots (exclude from auto-scheduling)
- [ ] Manual conflict visualization
- [ ] Scheduling confidence scores

---

## Implementation Checklist

- [ ] Add `isAdmin` flag or facilitator-mode check
- [ ] Implement `SchedulingService` with algorithm above
- [ ] Add `POST /api/admin/schedule` endpoint
- [ ] Add "Schedule Topics" button in admin UI
- [ ] Compute frozen slots based on current time
- [ ] Emit `DiscussionAction` events through existing WebSocket broadcast
- [ ] Add scheduling summary/toast notification
- [ ] Test with sample data: varied votes, conflicts, multi-day

---

## Open Questions

1. **Scheduling frequency** — Manual button only, or also periodic (e.g., every 10 min)?
2. **Notification** — When a user's interested topic moves, notify them?
3. **Conflict visibility** — Show users their personal conflicts in the UI?
