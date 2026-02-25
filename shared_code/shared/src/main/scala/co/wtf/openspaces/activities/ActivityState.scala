package co.wtf.openspaces.activities

import co.wtf.openspaces.Person
import neotype.unwrap

case class ActivityState(
  activities: Map[ActivityId, Activity],
):
  def ownedBy(person: Person): List[Activity] =
    activities.values
      .filter(_.creator == person)
      .toList
      .sortBy(activity => (activity.eventTime, -activity.interestCount, activity.createdAtEpochMs, activity.id.unwrap))

  def apply(action: ActivityActionConfirmed): ActivityState =
    action match
      case ActivityActionConfirmed.StateReplace(newActivities) =>
        copy(activities = newActivities.map(activity => activity.id -> activity).toMap)
      case ActivityActionConfirmed.Created(activity) =>
        copy(activities = activities + (activity.id -> activity))
      case ActivityActionConfirmed.InterestSet(activityId, person, interested, joinedAtEpochMs, newOwner) =>
        copy(activities = activities.updatedWith(activityId) {
          _.map { activity =>
            val updatedMembers =
              if interested then activity.withMember(person, joinedAtEpochMs.getOrElse(activity.createdAtEpochMs))
              else activity.withoutMember(person)
            newOwner.fold(updatedMembers)(updatedMembers.withOwner)
          }
        })
      case ActivityActionConfirmed.Updated(activityId, newDescription, newEventTime) =>
        copy(activities = activities.updatedWith(activityId) {
          _.map(activity =>
            activity.copy(
              description = newDescription,
              eventTime = newEventTime,
            ),
          )
        })
      case ActivityActionConfirmed.Deleted(activityId, _, _) =>
        copy(activities = activities - activityId)
      case ActivityActionConfirmed.SlackThreadLinked(activityId, slackThreadUrl) =>
        copy(activities = activities.updatedWith(activityId) {
          _.map(_.copy(slackThreadUrl = Some(slackThreadUrl)))
        })
      case ActivityActionConfirmed.Unauthorized(_) =>
        this
      case ActivityActionConfirmed.Rejected(_) =>
        this

object ActivityState:
  val empty: ActivityState = ActivityState(Map.empty)
