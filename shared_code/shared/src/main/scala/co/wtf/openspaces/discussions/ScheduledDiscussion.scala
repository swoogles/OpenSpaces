package co.wtf.openspaces.discussions

import co.wtf.openspaces.{Room, TimeSlot}
import zio.json.*

case class ScheduledDiscussion(
  discussion: Discussion,
  room: Room,
  timeSlot: TimeSlot)
    derives JsonCodec