package co.wtf.openspaces

import co.wtf.openspaces.DiscussionAction.Rename
import co.wtf.openspaces.VotePosition.Interested
import neotype.*
import neotype.given
import neotype.interop.zioschema.given
import neotype.interop.ziojson.given
import zio.schema.*
import zio.json.*

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

sealed trait WebSocketMessage derives JsonCodec, Schema
sealed trait WebSocketMessageFromClient
    extends WebSocketMessage
    derives JsonCodec, Schema
sealed trait WebSocketMessageFromServer
    extends WebSocketMessage
    derives JsonCodec, Schema

case class Ticket(
  uuid: UUID)
    extends WebSocketMessageFromClient
    derives JsonCodec, Schema

enum DiscussionAction derives JsonCodec:
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
  case ResetUser(
    person: Person)
  case Rename(
    topicId: TopicId,
    newTopic: Topic) // Any reason to pass original, now that I'm updating based on id?
  case SetRoomSlot(
    topicId: TopicId,
    expectedCurrentRoomSlot: Option[RoomSlot],
    newRoomSlot: Option[RoomSlot])
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
  case ResetUser(
    person: Person,
    deletedTopicIds: List[TopicId],
    clearedVoteTopicIds: List[TopicId])
  case Rename(
    topicId: TopicId,
    newTopic: Topic) // Any reason to pass original, now that I'm updating based on id?
  case SetRoomSlot(
    topicId: TopicId,
    newRoomSlot: Option[RoomSlot])
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
  case StateReplace(
    discussions: List[Discussion])
  case Unauthorized(
    discussionAction: DiscussionAction)
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
      case DiscussionAction.ResetUser(_) =>
        throw new Exception(
          "ResetUser requires server-side processing to determine affected topics.",
        )
      case DiscussionAction.Rename(topicId, newTopic) =>
        DiscussionActionConfirmed.Rename(topicId, newTopic)
      case DiscussionAction.SetRoomSlot(topicId, _, newRoomSlot) =>
        DiscussionActionConfirmed.SetRoomSlot(topicId, newRoomSlot)
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

enum LightningTalkAction derives JsonCodec:
  case SetParticipation(
    speaker: Person,
    participating: Boolean)
  case SetAssignment(
    proposalId: LightningTalkId,
    expectedCurrentAssignment: Option[LightningAssignment],
    newAssignment: Option[LightningAssignment])
  case DrawForNextNight

enum LightningTalkActionConfirmed derives JsonCodec:
  case AddResult(
    proposal: LightningTalkProposal)
  case SlackThreadLinked(
    proposalId: LightningTalkId,
    slackThreadUrl: String)
  case Delete(
    proposalId: LightningTalkId)
  case SetAssignment(
    proposalId: LightningTalkId,
    newAssignment: Option[LightningAssignment])
  case DrawForNightResult(
    night: LightningTalkNight,
    assignments: List[LightningDrawAssignment])
  case StateReplace(
    proposals: List[LightningTalkProposal])
  case Unauthorized(
    action: LightningTalkAction)
  case Rejected(
    action: LightningTalkAction)

case class DiscussionActionMessage(
  action: DiscussionAction,
) extends WebSocketMessageFromClient derives JsonCodec

case class DiscussionActionConfirmedMessage(
  event: DiscussionActionConfirmed,
) extends WebSocketMessageFromServer derives JsonCodec

case class LightningTalkActionMessage(
  action: LightningTalkAction,
) extends WebSocketMessageFromClient derives JsonCodec

case class LightningTalkActionConfirmedMessage(
  event: LightningTalkActionConfirmed,
) extends WebSocketMessageFromServer derives JsonCodec
