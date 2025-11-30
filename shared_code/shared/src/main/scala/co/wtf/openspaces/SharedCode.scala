package co.wtf.openspaces

import co.wtf.openspaces.DiscussionAction.Rename
import co.wtf.openspaces.VotePosition.Interested
import neotype.*
import neotype.given
import neotype.interop.ziojson.given
import zio.json.*

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

// TODO Conver to newType, like Topic, but without the length requirement
type Person = Person.Type
object Person extends Newtype[String]

enum VotePosition derives JsonCodec:
  case Interested, NotInterested

case class Feedback(
  voter: Person,
  position: VotePosition)
    derives JsonCodec

case class Discussion(
  topic: Topic,
  facilitator: Person,
  interestedParties: Set[Feedback],
  id: TopicId,
  glyphicon: Glyphicon,
  roomSlot: Option[RoomSlot])
    derives JsonCodec:

  val votes: Int = interestedParties.count(_.position == Interested)
  val facilitatorName = facilitator.unwrap
  val topicName = topic.unwrap

object Discussion:
  def apply(
    topic: Topic,
    facilitator: Person,
    id: TopicId,
    glyphicon: Glyphicon,
    roomSlot: Option[RoomSlot] = None,
  ): Discussion =
    Discussion(
      topic,
      facilitator,
      Set(Feedback(facilitator, Interested)),
      id,
      glyphicon,
      roomSlot,
    )

  val example1 = Discussion(
    Topic(
      "Continuous Deployment - A goal, an asymptote, or an ass out of you and me?",
    ),
    Person("Bill"),
    TopicId(1),
    GlyphiconUtils.names(1),
    roomSlot = Some(
      RoomSlot(Room.king,
               TimeSlot("8:00-8:50",
                        LocalDateTime.parse("2025-06-24T08:00:00"),
                        LocalDateTime.parse("2025-06-24T08:50:00"),
               ),
      ),
    ),
  )

  val example2 = Discussion(
    Topic("Managing emotional energy on the job"),
    Person("Emma"),
    TopicId(2),
    GlyphiconUtils.names(2),
    Some(
      RoomSlot(Room.artGallery,
               TimeSlot("8:00-8:50",
                        LocalDateTime.parse("2025-06-24T08:00:00"),
                        LocalDateTime.parse("2025-06-24T08:50:00"),
               ),
      ),
    ),
  )

  val example3 =
    Discussion(
      Topic("How to make a great cup of coffee"),
      Person("John"),
      TopicId(3),
      GlyphiconUtils.names(3),
      Some(
        RoomSlot(Room.king,
                 TimeSlot("9:20-10:10",
                          LocalDateTime.parse("2025-06-24T09:20:00"),
                          LocalDateTime.parse("2025-06-24T10:10:00"),
                 ),
        ),
      ),
    )

  val example4 =
    Discussion(
      Topic("How to make a great cup of tea"),
      Person("John"),
      Set(Feedback(Person("Bill"), VotePosition.NotInterested)),
      TopicId(4),
      GlyphiconUtils.names(4),
      None,
    )

  val example5 =
    Discussion(
      Topic("Fighting round the world"),
      Person("John"),
      Set.empty,
      TopicId(5),
      GlyphiconUtils.names(5),
      None,
    )

  val example6 =
    Discussion(
      Topic("Pandas - friends or foes?"),
      Person("John"),
      Set.empty,
      TopicId(5),
      GlyphiconUtils.names(6),
      None,
    )

sealed trait WebSocketMessage derives JsonCodec

case class Ticket(
  uuid: UUID)
    extends WebSocketMessage
    derives JsonCodec

enum DiscussionAction extends WebSocketMessage derives JsonCodec:
  case Add(
    topic: Topic,
    facilitator: Person)
  case Delete(
    topic: TopicId)
  case Vote(
    topic: TopicId,
    feedback: Feedback)
  case RemoveVote(
    topic: TopicId,
    voter: Person)
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

enum DiscussionActionConfirmed derives JsonCodec:
  case Delete(
    topic: TopicId)
  case Vote(
    topic: TopicId,
    feedback: Feedback)
  case RemoveVote(
    topic: TopicId,
    voter: Person)
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
  case AddResult(
    discussion: Discussion)
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
      case DiscussionAction.Delete(topic) =>
        DiscussionActionConfirmed.Delete(topic)
      case DiscussionAction.Vote(topic, feedback) =>
        DiscussionActionConfirmed.Vote(topic, feedback)
      case DiscussionAction.RemoveVote(topic, voter) =>
        DiscussionActionConfirmed.RemoveVote(topic, voter)
      case DiscussionAction.Rename(topicId, newTopic) =>
        DiscussionActionConfirmed.Rename(topicId, newTopic)
      case DiscussionAction.UpdateRoomSlot(topicId, roomSlot) =>
        DiscussionActionConfirmed.UpdateRoomSlot(topicId, roomSlot)
      case DiscussionAction.Unschedule(topicId) =>
        DiscussionActionConfirmed.Unschedule(topicId)
      case DiscussionAction.MoveTopic(topicId, targetRoomSlot) =>
        DiscussionActionConfirmed.MoveTopic(topicId, targetRoomSlot)

case class Room(
  id: Int,
  name: String,
  capacity: Int)
    derives JsonCodec

object Room:
  val king = Room(0, "King", 30)
  val artGallery = Room(1, "Art Gallery", 20)
  val hawk = Room(2, "Hawk", 15)
  val danceHall = Room(3, "Dance Hall", 10)

case class TimeSlot(
  s: String,
//                     id: String,
  startTime: LocalDateTime, // TODO Restore this. Required for dealing with topics showing up on multiple days
  endTime: LocalDateTime, // TODO Restore this.
) derives JsonCodec

case class ScheduledDiscussion(
  discussion: Discussion,
  room: Room,
  timeSlot: TimeSlot)
    derives JsonCodec

case class RoomSlot(
  room: Room,
  timeSlot: TimeSlot)
    derives JsonCodec:
  def displayString: String = timeSlot.s + " " + room.name

case class TimeSlotForAllRooms(
  time: TimeSlot,
  rooms: List[Room])
    derives JsonCodec

case class DaySlots(
  date: LocalDate,
  slots: List[TimeSlotForAllRooms])
    derives JsonCodec
