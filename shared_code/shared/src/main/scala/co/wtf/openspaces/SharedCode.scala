package co.wtf.openspaces

import co.wtf.openspaces.DiscussionAction.Rename
import zio.json.*

import java.util.UUID

case class Discussion(
                       topic: Topic,
                       facilitator: String, 
                       interestedParties: Set[String],
                       id: TopicId
                     ) derives JsonCodec:
  val votes: Int = interestedParties.size
object Discussion:

  val example1 = Discussion(
    Topic.parseOrDie("Continuous Deployment - A goal, or an asymptote?"),
    "Bill",
    Set("Bill"),
    TopicId(1)
  )

  val example2 = Discussion(
    Topic.parseOrDie(
      "Managing emotional energy on the job"),
    "Emma",
    Set("Emma"),
    TopicId(2)
  )


case class DiscussionState(
                          data: Map[TopicId, Discussion]
                          ):
  def apply(discussionAction: DiscussionAction): DiscussionState = {
    copy(data =
      discussionAction match
        case DiscussionAction.Delete(topicId) =>
          data.filterNot(_._2.id == topicId)
        case DiscussionAction.Add(discussion) =>
          data +(discussion.id -> discussion) // Only add if new topic title
        case DiscussionAction.Vote(topicId, voter) =>
          data.updatedWith(topicId){
            _.map(value =>
              value.copy(interestedParties = value.interestedParties + voter))
          }
        case DiscussionAction.RemoveVote(topicId, voter) =>
          data.updatedWith(topicId){
            _.map(value =>
              value.copy(interestedParties = value.interestedParties - voter))
          }
        case Rename(topicId, newTopic) =>
          data.updatedWith(topicId){
            _.map(value =>
              println("updating name")
              value.copy(topic = newTopic))
          }
    )
  }

object DiscussionState:
  def apply(input: Discussion*): DiscussionState =
    val startingState = Map(
      input.map(d => (d.id, d))*
    )
    DiscussionState(
      startingState
    )


enum DiscussionAction derives JsonCodec:
  case Delete(topic: TopicId)
  case Add(discussion: Discussion)
  case Vote(topic: TopicId, voter: String)
  case RemoveVote(topic: TopicId, voter: String)
  case Rename(topicId: TopicId, newTopic: Topic) // Any reason to pass original, now that I'm updating based on id?

enum Room:
  case King
  case ArtGallery
  case Hawk
  case DanceHall

case class ScheduleSlot(room: Room)