package co.wtf.openspaces

import co.wtf.openspaces.DiscussionAction.Rename
import zio.json.*

case class Person(unwrap: String)
object Person:
  given codec: JsonCodec[Person] = JsonCodec.string.transform[Person](s => Person(s), _.unwrap)

case class Discussion(
                       topic: Topic,
                       facilitator: Person, 
                       interestedParties: Set[Person],
                       id: TopicId
                     ) derives JsonCodec:
  val votes: Int = interestedParties.size
object Discussion:

  val example1 = Discussion(
    Topic.parseOrDie("Continuous Deployment - A goal, or an asymptote?"),
    Person("Bill"),
    Set(Person("Bill")),
    TopicId(1)
  )

  val example2 = Discussion(
    Topic.parseOrDie(
      "Managing emotional energy on the job"),
    Person("Emma"),
    Set(Person("Emma")),
    TopicId(2)
  )







enum DiscussionAction derives JsonCodec:
  case Delete(topic: TopicId)
  case Add(discussion: Discussion)
  case Vote(topic: TopicId, voter: Person)
  case RemoveVote(topic: TopicId, voter: Person)
  case Rename(topicId: TopicId, newTopic: Topic) // Any reason to pass original, now that I'm updating based on id?

enum Room:
  case King
  case ArtGallery
  case Hawk
  case DanceHall

case class ScheduleSlot(room: Room)