package co.wtf.openspaces

import zio.json.*

case class RoomSlot(
  room: Room,
  timeSlot: TimeSlot)
    derives JsonCodec:
  def displayString: String = timeSlot.displayString + " " + room.name
