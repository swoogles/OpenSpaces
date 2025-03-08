package co.wtf.openspaces

import co.wtf.openspaces.DiscussionAction.Rename
import co.wtf.openspaces.VotePosition.Interested
import zio.json.*

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









enum DiscussionAction derives JsonCodec:
  case Add(discussion: Discussion)
  case Delete(topic: TopicId)
  case Vote(topic: TopicId, feedback: Feedback)
  case RemoveVote(topic: TopicId, voter: Person)
  case Rename(topicId: TopicId, newTopic: Topic) // Any reason to pass original, now that I'm updating based on id?

enum Room:
  case King
  case ArtGallery
  case Hawk
  case DanceHall

case class ScheduleSlot(room: Room)