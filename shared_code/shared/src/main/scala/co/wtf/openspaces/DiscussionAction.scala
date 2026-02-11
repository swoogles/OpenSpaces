package co.wtf.openspaces

import co.wtf.openspaces.DiscussionAction.Rename
import co.wtf.openspaces.VotePosition.Interested
import neotype.*
import neotype.given
import neotype.interop.ziojson.given
import zio.json.*

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

sealed trait WebSocketMessage derives JsonCodec

case class Ticket(
  uuid: UUID)
    extends WebSocketMessage
    derives JsonCodec

enum DiscussionAction extends WebSocketMessage derives JsonCodec:
  case Add(
    topic: Topic,
    facilitator: Person)
  case AddWithRoomSlot(
    topic: Topic,
    facilitator: Person,
    roomSlot: RoomSlot)
  case Delete(
    topic: TopicId)
  case Vote(
    topic: TopicId,
    feedback: Feedback)
  case Rename(
    topicId: TopicId,
    newTopic: Topic) // Any reason to pass original, now that I'm updating based on id?
  case UpdateRoomSlot(
    topicId: TopicId,
    roomSlot: RoomSlot)
  case Unschedule(
    topicId: TopicId)
  case MoveTopic(
    topicId: TopicId,
    targetRoomSlot: RoomSlot)
  case SwapTopics(
    topic1: TopicId,
    expectedRoomSlot1: RoomSlot,
    topic2: TopicId,
    expectedRoomSlot2: RoomSlot)

enum DiscussionActionConfirmed derives JsonCodec:
  case Delete(
    topic: TopicId)
  case Vote(
    topic: TopicId,
    feedback: Feedback)
  case Rename(
    topicId: TopicId,
    newTopic: Topic) // Any reason to pass original, now that I'm updating based on id?
  case UpdateRoomSlot(
    topicId: TopicId,
    roomSlot: RoomSlot) // Any reason to pass original, now that I'm updating based on id?
  case Unschedule(
    topicId: TopicId) // TODO Should actually be an Option[RoomSlot], when unscheduling something
  case MoveTopic(
    topicId: TopicId,
    targetRoomSlot: RoomSlot)
  case SwapTopics(
    topic1: TopicId,
    newRoomSlot1: RoomSlot,
    topic2: TopicId,
    newRoomSlot2: RoomSlot)
  case AddResult(
    discussion: Discussion)
  case SlackThreadLinked(
    topicId: TopicId,
    slackThreadUrl: String)
  case Rejected(
    discussionAction: DiscussionAction)

object DiscussionActionConfirmed:
  def fromDiscussionAction(
    discussionAction: DiscussionAction,
  ): DiscussionActionConfirmed =
    discussionAction match
      case DiscussionAction.Add(topic, facilitator) =>
        throw new Exception(
          "This should not happen. You need to sort out your models.",
        )
      case DiscussionAction.AddWithRoomSlot(topic, facilitator, _) =>
        throw new Exception(
          "This should not happen. You need to sort out your models.",
        )
      case DiscussionAction.Delete(topic) =>
        DiscussionActionConfirmed.Delete(topic)
      case DiscussionAction.Vote(topic, feedback) =>
        DiscussionActionConfirmed.Vote(topic, feedback)
      case DiscussionAction.Rename(topicId, newTopic) =>
        DiscussionActionConfirmed.Rename(topicId, newTopic)
      case DiscussionAction.UpdateRoomSlot(topicId, roomSlot) =>
        DiscussionActionConfirmed.UpdateRoomSlot(topicId, roomSlot)
      case DiscussionAction.Unschedule(topicId) =>
        DiscussionActionConfirmed.Unschedule(topicId)
      case DiscussionAction.MoveTopic(topicId, targetRoomSlot) =>
        DiscussionActionConfirmed.MoveTopic(topicId, targetRoomSlot)
      case DiscussionAction.SwapTopics(topic1,
                                       expected1,
                                       topic2,
                                       expected2,
          ) =>
        // Note: The confirmed action contains the NEW room slots (swapped)
        DiscussionActionConfirmed.SwapTopics(topic1,
                                             expected2,
                                             topic2,
                                             expected1,
        )
