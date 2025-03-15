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
                       id: TopicId
                     ) derives JsonCodec:
  val votes: Int = interestedParties.count(_.position == Interested)

object Discussion:
  def apply(
             topic: Topic,
             facilitator: Person,
             id: TopicId
           ): Discussion =
    Discussion(
      topic,
      facilitator,
      Set(Feedback(facilitator, Interested)),
      id
    )

  val example1 = Discussion(
    Topic.parseOrDie("Continuous Deployment - A goal, or an asymptote?"),
    Person("Bill"),
    TopicId(1)
  )

  val example2 = Discussion(
    Topic.parseOrDie(
      "Managing emotional energy on the job"),
    Person("Emma"),
    TopicId(2)
  )

  val example3 =
    Discussion(
      Topic.parseOrDie("How to make a great cup of coffee"),
      Person("John"),
      TopicId(3)
    )

  val example4 =
    Discussion(
      Topic.parseOrDie("How to make a great cup of tea"),
      Person("John"),
      Set(Feedback(Person("Bill"), VotePosition.NotInterested)),
      TopicId(4)
    )
    
  val example5 =
    Discussion(
      Topic.parseOrDie("Fighting round the world"),
      Person("John"),
      Set.empty,
      TopicId(5)
    )

  val example6 =
    Discussion(
      Topic.parseOrDie("Pandas - friends or foes?"),
      Person("John"),
      Set.empty,
      TopicId(5)
    )








enum DiscussionAction derives JsonCodec:
  case Add(discussion: Discussion)
  case Delete(topic: TopicId)
  case Vote(topic: TopicId, feedback: Feedback)
  case RemoveVote(topic: TopicId, voter: Person)
  case Rename(topicId: TopicId, newTopic: Topic) // Any reason to pass original, now that I'm updating based on id?


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


case class ScheduleSlot(room: Room)

case class TimeSlot(
                     id: String,
                     startTime: LocalDateTime,
                     endTime: LocalDateTime
                   )derives JsonCodec

case class ScheduledDiscussion(
                                discussion: Discussion,
                                room: Room,
                                timeSlot: TimeSlot
                              ) derives JsonCodec