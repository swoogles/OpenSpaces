package co.wtf.openspaces

import zio.json.*
import zio.schema.{DeriveSchema, Schema}

case class Discussion(
                       topic: String, 
                       facilitator: String, 
                       interestedParties: Set[String]
                     ) derives JsonCodec:
  val votes: Int = interestedParties.size












sealed trait DiscussionAction
object DiscussionAction:
  case class Delete(topic: String) extends DiscussionAction derives JsonCodec
  case class Add(discussion: Discussion) extends DiscussionAction derives JsonCodec
  case class Vote(topic: String, voter: String) extends DiscussionAction derives JsonCodec
  case class RemoveVote(topic: String, voter: String) extends DiscussionAction derives JsonCodec
  implicit val codec: JsonCodec[DiscussionAction] = JsonCodec.derived[DiscussionAction]
  implicit val schema: Schema[DiscussionAction] =  DeriveSchema.gen

  def foo(discussionAction: DiscussionAction, currentDiscussions: List[Discussion]) = {
    discussionAction match
      case DiscussionAction.Delete(topic) =>
        currentDiscussions.filterNot(_.topic == topic)
      case DiscussionAction.Add(discussion) =>
        currentDiscussions :+ discussion // Only add if new topic title
      case DiscussionAction.Vote(topic, voter) =>
        currentDiscussions.map {
          discussion =>
            if (discussion.topic == topic)
              println("Bumping the count")
              discussion.copy(interestedParties = discussion.interestedParties + voter)
            else
              discussion
        }
      case DiscussionAction.RemoveVote(topic, voter) =>
        currentDiscussions.map {
          discussion =>
            if (discussion.topic == topic)
              println("Removing the count")
              discussion.copy(interestedParties = discussion.interestedParties - voter)
            else
              discussion
        }
  }


// Some day
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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