package co.wtf.openspaces

import zio.json.*

//sealed trait DiscussionAction:
//  case class Delete(topic: String) extends DiscussionAction derives JsonCodec
//  case class Add(discussion: Discussion) extends DiscussionAction derives JsonCodec


sealed trait DiscussionAction
object DiscussionAction:
  case class Delete(topic: String) extends DiscussionAction derives JsonCodec
  case class Add(discussion: Discussion) extends DiscussionAction derives JsonCodec
  case class Vote(topic: String) extends DiscussionAction derives JsonCodec
  implicit val codec: JsonCodec[DiscussionAction] = JsonCodec.derived[DiscussionAction]


case class Discussion(topic: String, votes: Int, facilitator: String) derives JsonCodec

enum AppView:
  case Home
  case ScheduleView
  case SubmitTopic

enum Room:
  case King
  case ArtGallery
  case Hawk
  case DanceHall

case class ScheduleSlot(room: Room)