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

  val example1 = Discussion(
    Topic.parseOrDie("Continuous Deployment - A goal, or an asymptote?"),
    Person("Bill"),
    Set(Feedback(Person("Bill"), VotePosition.Interested)),
    TopicId(1)
  )

  val example2 = Discussion(
    Topic.parseOrDie(
      "Managing emotional energy on the job"),
    Person("Emma"),
    Set(Feedback(Person("Emma"), VotePosition.Interested)),
    TopicId(2)
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