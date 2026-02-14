package co.wtf.openspaces.util

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import co.wtf.openspaces.RoomSlot

/** Tracks the DOM positions of each schedule slot so animations can
  * start from the actual on-screen location rather than an estimated
  * grid slot.
  */
object SlotPositionTracker:
  private val slotElements: Var[Map[RoomSlot, dom.Element]] = Var(
    Map.empty,
  )
  // Keep last known centers so animations still have a reasonable start
  // point even if a slot temporarily unmounts (e.g., virtualization or
  // view changes).
  private val lastKnownCenters: Var[Map[RoomSlot, (Double, Double)]] =
    Var(
      Map.empty,
    )
  
  // Timestamp of last bulk position update to throttle reflows
  private var lastBulkUpdateMs: Double = 0
  private val BulkUpdateThrottleMs: Double = 100 // Only refresh positions every 100ms max

  /** Register a slot element so we can compute its live center
    * position.
    *
    * @return
    *   a cleanup function to unregister on unmount
    */
  def register(
    slot: RoomSlot,
    element: dom.Element,
  ): () => Unit =
    slotElements.update(_ + (slot -> element))
    // Capture an initial center right away (only on registration, not every access)
    updateCenterNoThrottle(slot, element)
    () =>
      slotElements.update(_ - slot)
      () // keep lastKnownCenters for fallback

  /** Current center point for a slot, if its element is mounted.
    * Uses cached positions to avoid layout thrashing.
    */
  def center(
    slot: RoomSlot,
  ): Option[(Double, Double)] =
    // First try cached position (no reflow)
    lastKnownCenters.now().get(slot).orElse {
      // Only compute fresh if not cached
      slotElements
        .now()
        .get(slot)
        .flatMap(el => updateCenterNoThrottle(slot, el))
    }

  /** Force refresh all slot positions. Call this sparingly (e.g., on resize). */
  def refreshAllPositions(): Unit =
    val now = System.currentTimeMillis().toDouble
    if now - lastBulkUpdateMs > BulkUpdateThrottleMs then
      lastBulkUpdateMs = now
      slotElements.now().foreach { case (slot, element) =>
        updateCenterNoThrottle(slot, element)
      }

  private def updateCenterNoThrottle(
    slot: RoomSlot,
    element: dom.Element,
  ): Option[(Double, Double)] =
    val rect = element.getBoundingClientRect()
    val center =
      (rect.left + rect.width / 2, rect.top + rect.height / 2)
    lastKnownCenters.update(_ + (slot -> center))
    Some(center)

  /** Pixel delta from one slot center to another, if both are known.
    */
  def offsetBetween(
    fromSlot: RoomSlot,
    toSlot: RoomSlot,
  ): Option[(Double, Double)] =
    for
      from <- center(fromSlot)
      to   <- center(toSlot)
    yield (from._1 - to._1, from._2 - to._2)
