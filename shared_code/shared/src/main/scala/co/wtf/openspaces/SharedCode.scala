package co.wtf.openspaces

import zio.json.*
import zio.schema.{DeriveSchema, Schema}
import neotype.*
import neotype.interop.ziojson.given

case class Topic(unwrap: String)
object Topic:
  def parse(raw: String) =
    Either.cond(
      raw.trim.length >= 10,
      Topic(raw),
      "Topic must be at least 10 characters long"
    )
  def parseOrDie(raw: String) =
    parse(raw).getOrElse(throw new Exception("Failed to parse topic: " + raw))
    
  given codec: JsonCodec[Topic] = JsonCodec.string.transformOrFail[Topic](s => Topic.parse(s), _.unwrap)

case class Discussion(
                       topic: Topic,
                       facilitator: String, 
                       interestedParties: Set[String]
                     ) derives JsonCodec:
  val votes: Int = interestedParties.size


enum DiscussionAction derives JsonCodec:
  case Delete(topic: Topic)
  case Add(discussion: Discussion)
  case Vote(topic: Topic, voter: String)
  case RemoveVote(topic: Topic, voter: String)

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
              discussion.copy(interestedParties = discussion.interestedParties + voter)
            else
              discussion
        }
      case DiscussionAction.RemoveVote(topic, voter) =>
        currentDiscussions.map {
          discussion =>
            if (discussion.topic == topic)
              discussion.copy(interestedParties = discussion.interestedParties - voter)
            else
              discussion
        }
  }

enum Room:
  case King
  case ArtGallery
  case Hawk
  case DanceHall

case class ScheduleSlot(room: Room)