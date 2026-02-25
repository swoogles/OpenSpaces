package co.wtf.openspaces.activities

import co.wtf.openspaces.Person
import neotype.interop.ziojson.given
import zio.json.*
import java.time.LocalDateTime

enum ActivityAction derives JsonCodec:
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

enum ActivityActionConfirmed derives JsonCodec:
  case Created(
    activity: Activity)
  case InterestSet(
    activityId: ActivityId,
    person: Person,
    interested: Boolean)
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
