package co.wtf.openspaces.discussions

import co.wtf.openspaces.{Room, TimeSlot}
import zio.json.*

case class TimeSlotForAllRooms(
  time: TimeSlot,
  rooms: List[Room])
    derives JsonCodec