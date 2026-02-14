package co.wtf.openspaces.util

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom.window
import co.wtf.openspaces.{TopicId, RoomSlot, TimeSlot}

/** Tracks active swap animations for smooth position transitions.
  *
  * Animation flow:
  *   1. When a swap/move is confirmed, we set initial offsets (from
  *      old position to new) 2. After a brief delay (one animation
  *      frame), we clear the offsets to (0, 0) 3. The spring
  *      animation interpolates from the offset to (0, 0), creating
  *      the sliding effect 4. Cleanup happens after the spring
  *      settles, but only if no newer animation has started for that
  *      topic
  */
object SwapAnimationState:
  // Stores the INITIAL offsets for each topic (where they should appear to come FROM)
  val initialOffsets: Var[Map[TopicId, (Double, Double)]] = Var(
    Map.empty,
  )

  // Stores whether each topic's animation should be "animating to zero"
  val animatingToZero: Var[Set[TopicId]] = Var(Set.empty)

  // Tracks animation "generation" per topic to prevent stale cleanup
  // Each new animation increments the generation, and cleanup only runs
  // if the generation hasn't changed (i.e., no newer animation started)
  private val animationGeneration: Var[Map[TopicId, Int]] = Var(
    Map.empty,
  )

  /** Immediately clean up all animation state for a topic.
    * Call this when a topic is deleted to prevent memory leaks.
    */
  def cleanupTopic(topicId: TopicId): Unit =
    initialOffsets.update(_ - topicId)
    animatingToZero.update(_ - topicId)
    animationGeneration.update(_ - topicId)

  /** Spring animation typically needs ~600-800ms to settle */
  val cleanupDelayMs: Int = 1000

  // Grid cell dimensions for offset calculations
  private val cellWidth = 60.0 // Approximate cell width in pixels
  // Use a smaller vertical offset since row calculations can over-estimate
  // the visual distance for adjacent time slots
  private val cellHeight = 40.0

  /** Calculates the pixel offset between two room slots */
  private def calculateOffset(
    fromSlot: RoomSlot,
    toSlot: RoomSlot,
  ): (Double, Double) =
    SlotPositionTracker.offsetBetween(fromSlot, toSlot).getOrElse {
      val colDiff = fromSlot.room.id - toSlot.room.id
      val rowDiff = timeSlotToRow(fromSlot.timeSlot) - timeSlotToRow(
        toSlot.timeSlot,
      )
      (colDiff * cellWidth, rowDiff * cellHeight)
    }

  /** Increments and returns the new animation generation for a topic
    */
  private def nextGeneration(
    topicId: TopicId,
  ): Int =
    val newGen = animationGeneration.now().getOrElse(topicId, 0) + 1
    animationGeneration.update(_ + (topicId -> newGen))
    newGen

  /** Cleans up animation state for a topic, but only if the
    * generation matches
    */
  private def cleanupIfSameGeneration(
    topicId: TopicId,
    expectedGen: Int,
  ): Unit =
    if (
      animationGeneration.now().get(topicId).contains(expectedGen)
    ) {
      initialOffsets.update(_ - topicId)
      animatingToZero.update(_ - topicId)
      animationGeneration.update(_ - topicId)
    }
    // If generation doesn't match, a newer animation is in progress - don't cleanup

  /** Starts an animation for a single topic being moved from one slot
    * to another
    */
  def startMoveAnimation(
    topicId: TopicId,
    fromSlot: RoomSlot,
    toSlot: RoomSlot,
  ): Unit =
    // Calculate offset: topic is now at toSlot, but should appear to come FROM fromSlot
    val offset = calculateOffset(fromSlot, toSlot)

    // Get a new generation number for this animation
    val gen = nextGeneration(topicId)

    // Step 1: Set initial offset (topic appears at its OLD position)
    initialOffsets.update(_ + (topicId -> offset))

    // Step 2: After a brief delay, trigger animation to zero
    window.setTimeout(
      () => animatingToZero.update(_ + topicId),
      16, // ~1 animation frame
    )

    // Step 3: Clean up after animation completes (only if no newer animation started)
    window.setTimeout(
      () => cleanupIfSameGeneration(topicId, gen),
      cleanupDelayMs,
    )

  /** Starts a swap animation between two topics based on their
    * RoomSlots
    */
  def startSwapAnimation(
    topic1: TopicId,
    slot1: RoomSlot,
    topic2: TopicId,
    slot2: RoomSlot,
  ): Unit =
    // After swap, topic1 is now at slot1, but WAS at slot2 before
    // So topic1 needs offset from slot2 to slot1
    val topic1Offset = calculateOffset(slot2, slot1)
    val topic2Offset = calculateOffset(slot1, slot2)

    // Get new generation numbers for both topics
    val gen1 = nextGeneration(topic1)
    val gen2 = nextGeneration(topic2)

    // Step 1: Set initial offsets (topics appear at their OLD positions)
    initialOffsets.update(
      _ + (topic1 -> topic1Offset) + (topic2 -> topic2Offset),
    )

    // Step 2: After a brief delay, trigger animation to zero
    window.setTimeout(
      () => animatingToZero.update(_ + topic1 + topic2),
      16, // ~1 animation frame
    )

    // Step 3: Clean up after animation completes (only if no newer animations started)
    window.setTimeout(
      () => {
        cleanupIfSameGeneration(topic1, gen1)
        cleanupIfSameGeneration(topic2, gen2)
      },
      cleanupDelayMs,
    )

  /** Gets the animated offset signal for a topic. Returns the initial
    * offset if not yet animating, or (0,0) if animating to final
    * position. The spring will interpolate between these values.
    */
  def getOffsetSignal(
    topicId: TopicId,
  ): Signal[(Double, Double)] =
    initialOffsets.signal.combineWith(animatingToZero.signal).map {
      case (offsets, animating) =>
        if (animating.contains(topicId)) {
          // Target position: animate TO (0, 0)
          (0.0, 0.0)
        }
        else {
          // Initial position: offset from old position
          offsets.getOrElse(topicId, (0.0, 0.0))
        }
    }

  /** Helper to convert TimeSlot to a row index for offset calculation
    */
  private def timeSlotToRow(
    timeSlot: TimeSlot,
  ): Int =
    val startTime = timeSlot.startTime
    val dayOffset = startTime.getDayOfYear * 100
    val timeOffset = startTime.getHour * 60 + startTime.getMinute
    (dayOffset + timeOffset) / 50
