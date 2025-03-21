package co.wtf.openspaces

import co.wtf.openspaces.DiscussionAction.Rename
import co.wtf.openspaces.VotePosition.Interested
import zio.json.*

import java.time.LocalDateTime

case class Person(unwrap: String)
object Person:
  given codec: JsonCodec[Person] = JsonCodec.string.transform[Person](s => Person(s), _.unwrap)

enum VotePosition derives JsonCodec:
  case Interested, NotInterested

case class Feedback(
                 voter: Person,
                 position: VotePosition
               ) derives JsonCodec

case class Discussion(
                       topic: Topic,
                       facilitator: Person,
                       interestedParties: Set[Feedback],
                       id: TopicId,
                       glyphicon: Glyphicon,
                       roomSlot: Option[RoomSlot] = None
                     ) derives JsonCodec:
  val votes: Int = interestedParties.count(_.position == Interested)

object Discussion:
  def apply(
             topic: Topic,
             facilitator: Person,
             id: TopicId,
              glyphicon: Glyphicon
           ): Discussion =
    Discussion(
      topic,
      facilitator,
      Set(Feedback(facilitator, Interested)),
      id,
      glyphicon
    )

  val example1 = Discussion(
    Topic.parseOrDie("Continuous Deployment - A goal, an asymptote, or an ass out of you and me?"),
    Person("Bill"),
    TopicId(1),
    GlyphiconUtils.names(1)
  )

  val example2 = Discussion(
    Topic.parseOrDie(
      "Managing emotional energy on the job"),
    Person("Emma"),
    TopicId(2),
    GlyphiconUtils.names(2)
  )

  val example3 =
    Discussion(
      Topic.parseOrDie("How to make a great cup of coffee"),
      Person("John"),
      TopicId(3),
      GlyphiconUtils.names(3)
    )

  val example4 =
    Discussion(
      Topic.parseOrDie("How to make a great cup of tea"),
      Person("John"),
      Set(Feedback(Person("Bill"), VotePosition.NotInterested)),
      TopicId(4),
      GlyphiconUtils.names(4)
    )
    
  val example5 =
    Discussion(
      Topic.parseOrDie("Fighting round the world"),
      Person("John"),
      Set.empty,
      TopicId(5),
      GlyphiconUtils.names(5)
    )

  val example6 =
    Discussion(
      Topic.parseOrDie("Pandas - friends or foes?"),
      Person("John"),
      Set.empty,
      TopicId(5),
      GlyphiconUtils.names(6)
    )








/* TODO Split this into at least:
    - User-submitted actions
      - These can be considered "attempts" and can be rejected
    - Server-side updates
      - These are authoritative, they will always update Client-side state
*/
enum DiscussionAction derives JsonCodec:
  case Add(
            topic: Topic,
            facilitator: Person,
            )
  case Delete(topic: TopicId)
  case Vote(topic: TopicId, feedback: Feedback)
  case RemoveVote(topic: TopicId, voter: Person)
  case Rename(topicId: TopicId, newTopic: Topic) // Any reason to pass original, now that I'm updating based on id?


enum DiscussionActionConfirmed derives JsonCodec:
  case Delete(topic: TopicId)
  case Vote(topic: TopicId, feedback: Feedback)
  case RemoveVote(topic: TopicId, voter: Person)
  case Rename(topicId: TopicId, newTopic: Topic) // Any reason to pass original, now that I'm updating based on id?
  case AddResult(discussion: Discussion)

object DiscussionActionConfirmed:
  def fromDiscussionAction(discussionAction: DiscussionAction): DiscussionActionConfirmed =
    discussionAction match
      case DiscussionAction.Delete(topic) => DiscussionActionConfirmed.Delete(topic)
      case DiscussionAction.Vote(topic, feedback) => DiscussionActionConfirmed.Vote(topic, feedback)
      case DiscussionAction.RemoveVote(topic, voter) => DiscussionActionConfirmed.RemoveVote(topic, voter)
      case DiscussionAction.Rename(topicId, newTopic) => DiscussionActionConfirmed.Rename(topicId, newTopic)
      case other => throw new Exception(s"Unexpected discussion action: $other")


case class Room(
                 id: Int,
                 name: String,
                 capacity: Int
               ) derives JsonCodec

object Room:
  val king = Room(0, "King", 30)
  val artGallery = Room(1, "Art Gallery", 20)
  val hawk = Room(2, "Hawk", 15)
  val danceHall = Room(3, "Dance Hall", 10)


case class TimeSlot(
                   s: String
//                     id: String,
//                     startTime: LocalDateTime,
//                     endTime: LocalDateTime
                   )derives JsonCodec

case class ScheduledDiscussion(
                                discussion: Discussion,
                                room: Room,
                                timeSlot: TimeSlot
                              ) derives JsonCodec

case class RoomSlot(
                                room: Room,
                                timeSlot: TimeSlot
                              )derives JsonCodec

case class FullSchedule(
                       slots: List[TimeSlotForAllRooms]
                       ) derives JsonCodec:
  def findDiscussion(searchTarget: Discussion): Option[ScheduledDiscussion] =
    slots.flatMap(slot => slot.findDiscussion(searchTarget)).headOption

case class TimeSlotForAllRooms(
                           time: TimeSlot,
                           cells: List[ScheduleSlot]
                       ) derives JsonCodec:
  def findDiscussion(searchTarget: Discussion): Option[ScheduledDiscussion] =
    cells.flatMap(cell => cell.withDiscussion(searchTarget))
      .headOption
      .map(filledCell => ScheduledDiscussion(searchTarget, filledCell.room, time))

case class ScheduleSlot(
                         room: Room,
                         discussion: Option[Discussion] = None
                       ) derives JsonCodec:
  def withDiscussion(searchTarget: Discussion): Option[FilledCell] =
    discussion.filter(_ == searchTarget)
      .map( d => FilledCell(room, d))


case class FilledCell(
                         room: Room,
                         discussion: Discussion
                       ) derives JsonCodec
