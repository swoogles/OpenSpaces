package co.wtf.openspaces

import zio.json.*
import java.time.LocalDateTime

case class TimeSlot(
  s: String,
//                     id: String,
  startTime: LocalDateTime, // TODO Restore this. Required for dealing with topics showing up on multiple days
  endTime: LocalDateTime, // TODO Restore this.
) derives JsonCodec