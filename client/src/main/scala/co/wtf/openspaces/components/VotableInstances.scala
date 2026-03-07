package co.wtf.openspaces.components

import co.wtf.openspaces.{Person, TopicId}
import co.wtf.openspaces.discussions.Discussion
import co.wtf.openspaces.activities.{Activity, ActivityId}
import neotype.unwrap

/** Votable typeclass instances for domain types. */
object VotableInstances:

  /** Discussion (Topics) voting - user has voted if they're in interestedParties */
  given Votable[Discussion, TopicId] with
    extension (item: Discussion)
      def votableId: TopicId = item.id

      def hasVotedOn(user: Person): Boolean =
        item.interestedParties.exists(_.voter == user)

      def sortKey: (Long, String) =
        (item.createdAtEpochMs, item.id.unwrap.toString)

  /** Activity voting - user has voted if they're a member */
  given Votable[Activity, ActivityId] with
    extension (item: Activity)
      def votableId: ActivityId = item.id

      def hasVotedOn(user: Person): Boolean =
        item.hasMember(user)

      def sortKey: (Long, String) =
        (item.createdAtEpochMs, item.id.unwrap.toString)
