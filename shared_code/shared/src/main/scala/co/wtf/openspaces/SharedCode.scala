package co.wtf.openspaces

import zio.json.*
import zio.schema.{DeriveSchema, Schema}

case class Discussion(
                       topic: String, 
                       facilitator: String, 
                       interestedParties: Set[String]
                     ) derives JsonCodec:
  val votes: Int = interestedParties.size












enum DiscussionAction derives JsonCodec:
  case Delete(topic: String)
  case Add(discussion: Discussion)
  case Vote(topic: String, voter: String)
  case RemoveVote(topic: String, voter: String)

object DiscussionAction:
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

// TODO Scala 3 enum version of DiscussionAction
enum DiscussionAction3 derives JsonCodec:
  case Delete(topic: String)
  case Add(discussion: Discussion)
  case Vote(topic: String, voter: String)
  case RemoveVote(topic: String, voter: String)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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