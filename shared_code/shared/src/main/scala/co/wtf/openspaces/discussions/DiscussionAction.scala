package co.wtf.openspaces.discussions

import co.wtf.openspaces.{Person, Topic, TopicId, RoomSlot}
import neotype.*
import neotype.given
import neotype.interop.zioschema.given
import neotype.interop.ziojson.given
import zio.schema.*
import zio.json.*
import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

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
  case SetLockedTimeslot(
    topicId: TopicId,
    expectedCurrentLockedTimeslot: Boolean,
    newLockedTimeslot: Boolean)
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
  case SetLockedTimeslot(
    topicId: TopicId,
    lockedTimeslot: Boolean)
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
      case DiscussionAction.SetLockedTimeslot(topicId,
                                              _,
                                              newLockedTimeslot,
          ) =>
        DiscussionActionConfirmed.SetLockedTimeslot(topicId,
                                                    newLockedTimeslot,
        )
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
