package co.wtf.openspaces

import zio.json.*

case class Discussion(
                       topic: Topic,
                       facilitator: String, 
                       interestedParties: Set[String]
                     ) derives JsonCodec:
  val votes: Int = interestedParties.size


case class DiscussionState(
                          data: Map[Topic, Discussion]
                          ):
  def apply(discussionAction: DiscussionAction): DiscussionState = {
    copy(data =
      discussionAction match
        case DiscussionAction.Delete(topic) =>
          data.filterNot(_._2.topic == topic)
        case DiscussionAction.Add(discussion) =>
          data +(discussion.topic -> discussion) // Only add if new topic title
        case DiscussionAction.Vote(topic, voter) =>
          data.map {
            (topic, discussion) =>
              (
                topic,
                if (discussion.topic == topic)
                  discussion.copy(interestedParties = discussion.interestedParties + voter)
                else
                  discussion
              )
          }
        case DiscussionAction.RemoveVote(topic, voter) =>
          data.map {
            (topic, discussion) =>
              (topic,
                if (discussion.topic == topic)
                  discussion.copy(interestedParties = discussion.interestedParties - voter)
                else
                  discussion
              )
          }
    )
  }

object DiscussionState:
  def apply(input: Discussion*): DiscussionState =
    val startingState = Map(
      input.map(d => (d.topic, d))*
    )
    DiscussionState(
      startingState
    )


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