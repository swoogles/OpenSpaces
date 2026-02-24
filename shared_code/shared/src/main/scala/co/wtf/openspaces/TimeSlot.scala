package co.wtf.openspaces

import zio.json.*
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

/** A time slot for scheduling discussions.
  * Equality is based on startTime and endTime only (not database IDs).
  */
case class TimeSlot(
  startTime: LocalDateTime,
  endTime: LocalDateTime,
) derives JsonCodec:
  
  /** Display string derived from start/end times, e.g. "9:00-9:50" */
  def displayString: String =
    val formatter = DateTimeFormatter.ofPattern("H:mm")
    s"${startTime.format(formatter)}-${endTime.format(formatter)}"
