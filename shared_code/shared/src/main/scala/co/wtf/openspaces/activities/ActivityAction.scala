package co.wtf.openspaces.activities

import co.wtf.openspaces.{ActorAction, HasActor, Person}
import neotype.interop.ziojson.given
import zio.json.*
import java.time.LocalDateTime

enum ActivityAction extends ActorAction derives JsonCodec:
  case Create(
    description: ActivityDescription,
    eventTime: LocalDateTime,
    creator: Person)
  case SetInterest(
    activityId: ActivityId,
    person: Person,
    interested: Boolean)
  case Update(
    activityId: ActivityId,
    newDescription: ActivityDescription,
    newEventTime: LocalDateTime,
    editor: Person)
  case Delete(
    activityId: ActivityId,
    requester: Person)

  def actor: Person = this match
    case Create(_, _, creator)    => creator
    case SetInterest(_, person, _) => person
    case Update(_, _, _, editor)  => editor
    case Delete(_, requester)     => requester

enum ActivityActionConfirmed extends HasActor derives JsonCodec:
  case Created(
    activity: Activity)
  case InterestSet(
    activityId: ActivityId,
    person: Person,
    interested: Boolean,
    joinedAtEpochMs: Option[Long],
    newOwner: Option[Person])
  case Updated(
    activityId: ActivityId,
    newDescription: ActivityDescription,
    newEventTime: LocalDateTime)
  case Deleted(
    activityId: ActivityId,
    slackChannelId: Option[String],
    slackThreadTs: Option[String])
  case SlackThreadLinked(
    activityId: ActivityId,
    slackThreadUrl: String)
  case StateReplace(
    activities: List[Activity])
  case Unauthorized(
    action: ActivityAction)
  case Rejected(
    action: ActivityAction)

  def actor: Option[Person] = this match
    case Created(activity) => Some(activity.creator)
    case InterestSet(_, person, _, _, _) => Some(person)
    case _ => None
