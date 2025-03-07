package co.wtf.openspaces

import co.wtf.openspaces.DiscussionAction.Rename
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
          data.updatedWith(topic){
            _.map(value =>
              value.copy(interestedParties = value.interestedParties + voter))
          }
        case DiscussionAction.RemoveVote(topic, voter) =>
          data.updatedWith(topic){
            _.map(value =>
              value.copy(interestedParties = value.interestedParties - voter))
          }
        case Rename(originalTopic, newTopic) =>
          data.updatedWith(originalTopic){
            _.map(value =>
              value.copy(topic = newTopic))
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
  case Rename(originalTopic: Topic, newTopic: Topic)

enum Room:
  case King
  case ArtGallery
  case Hawk
  case DanceHall

case class ScheduleSlot(room: Room)