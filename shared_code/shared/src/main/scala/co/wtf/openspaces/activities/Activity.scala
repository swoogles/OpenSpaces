package co.wtf.openspaces.activities

import co.wtf.openspaces.Person
import neotype.*
import neotype.unwrap
import neotype.interop.ziojson.given
import zio.json.*
import java.time.LocalDateTime

type ActivityId = ActivityId.Type
object ActivityId extends Newtype[Long]

type ActivityDescription = ActivityDescription.Type
object ActivityDescription extends Newtype[String]:
  override inline def validate(value: String) =
    val trimmed = value.trim
    if trimmed.length < 3 then "Activity description must be at least 3 characters"
    else if trimmed.length > 120 then "Activity description must be 120 characters or less"
    else true

case class ActivityMember(
  person: Person,
  joinedAtEpochMs: Long,
) derives JsonCodec

object ActivityMember:
  given Ordering[ActivityMember] = Ordering.by(_.joinedAtEpochMs)

case class Activity(
  id: ActivityId,
  description: ActivityDescription,
  creator: Person,
  creatorDisplayName: Option[String],
  eventTime: LocalDateTime,
  members: List[ActivityMember],
  createdAtEpochMs: Long,
  slackThreadUrl: Option[String] = None,
  dismissedBy: Set[Person] = Set.empty,
) derives JsonCodec:
  val descriptionText: String = description.unwrap
  val creatorName: String = creatorDisplayName.getOrElse(creator.unwrap)
  val interestCount: Int = members.size

  def hasMember(person: Person): Boolean =
    members.exists(_.person == person)

  def hasDismissed(person: Person): Boolean =
    dismissedBy.contains(person)

  /** Returns true if the user has voted (either interested or dismissed) */
  def hasVoted(person: Person): Boolean =
    hasMember(person) || hasDismissed(person)

  def isOwner(person: Person): Boolean =
    creator == person

  def nextOwner: Option[Person] =
    members
      .filterNot(_.person == creator)
      .sorted
      .headOption
      .map(_.person)

  def wouldBeDeletedIfLeaves(person: Person): Boolean =
    isOwner(person) && nextOwner.isEmpty

  def withMember(person: Person, joinedAtEpochMs: Long): Activity =
    if hasMember(person) then this
    else copy(
      members = members :+ ActivityMember(person, joinedAtEpochMs),
      dismissedBy = dismissedBy - person, // Remove from dismissed if they become interested
    )

  def withoutMember(person: Person): Activity =
    copy(members = members.filterNot(_.person == person))

  def withDismissal(person: Person): Activity =
    if hasDismissed(person) then this
    else copy(
      dismissedBy = dismissedBy + person,
      members = members.filterNot(_.person == person), // Remove from members if they dismiss
    )

  def withoutDismissal(person: Person): Activity =
    copy(dismissedBy = dismissedBy - person)

  def withOwner(newOwner: Person): Activity =
    copy(creator = newOwner)
