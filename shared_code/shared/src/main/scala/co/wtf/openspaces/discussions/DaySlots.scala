package co.wtf.openspaces.discussions

import zio.json.*
import java.time.LocalDate

case class DaySlots(
  date: LocalDate,
  slots: List[TimeSlotForAllRooms])
    derives JsonCodec