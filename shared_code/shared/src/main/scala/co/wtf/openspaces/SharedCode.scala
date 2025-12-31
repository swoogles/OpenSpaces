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

/** Represents the user's voting state for a topic, used for styling.
  *   - VotedFor: User has voted in favor (most visible)
  *   - NotVoted: User has not voted (neutral)
  *   - VotedAgainst: User has voted against (least visible)
  */
enum VotingState:
  case VotedFor, NotVoted, VotedAgainst

  /** Returns the CSS class name for this voting state */
  def cssClass: String = this match
    case VotedFor      => "vote-state-for"
    case NotVoted      => "vote-state-neutral"
    case VotedAgainst  => "vote-state-against"

  /** Returns an accessible label describing this voting state */
  def ariaLabel: String = this match
    case VotedFor      => "You voted for this topic"
    case NotVoted      => "You have not voted on this topic"
    case VotedAgainst  => "You voted against this topic"

object VotingState:
  /** Determines the voting state for a given user and discussion */
  def forUser(
    discussion: Discussion,
    user: Person,
  ): VotingState =
    discussion.interestedParties.find(_.voter == user) match
      case Some(feedback) =>
        feedback.position match
          case VotePosition.Interested    => VotedFor
          case VotePosition.NotInterested => VotedAgainst
      case None => NotVoted

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

/** A segment in a saved trip - represents one session to attend */
case class TripSegment(
  roomSlot: RoomSlot,
  topicName: String,
  facilitatorName: String,
  glyphicon: Glyphicon)
    derives JsonCodec

/** A saved trip containing multiple session segments */
case class SavedTrip(
  id: String,
  name: String,
  segments: List[TripSegment],
  createdAt: Long)
    derives JsonCodec

object SavedTrip:
  /** Generate a simple unique ID suitable for client-side use.
    * Uses timestamp + random component for uniqueness.
    */
  private def generateId(): String =
    val timestamp = System.currentTimeMillis()
    val random = scala.util.Random.nextInt(1000000)
    s"trip_${timestamp}_$random"

  def fromDiscussions(
    name: String,
    discussions: List[Discussion],
  ): SavedTrip =
    val segments = discussions
      .filter(_.roomSlot.isDefined)
      .sortBy(d => d.roomSlot.get.timeSlot.startTime.toString)
      .map(d =>
        TripSegment(
          d.roomSlot.get,
          d.topicName,
          d.facilitatorName,
          d.glyphicon,
        ),
      )
    SavedTrip(
      id = generateId(),
      name = name,
      segments = segments,
      createdAt = System.currentTimeMillis(),
    )

/** Container for saved trips with version for migration handling */
case class SavedTripsData(
  version: Int,
  trips: List[SavedTrip])
    derives JsonCodec

object SavedTripsData:
  val currentVersion = 1
  val empty: SavedTripsData = SavedTripsData(currentVersion, List.empty)
