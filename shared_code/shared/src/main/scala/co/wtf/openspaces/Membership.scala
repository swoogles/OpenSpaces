package co.wtf.openspaces

import neotype.interop.ziojson.given
import zio.json.*

/** Someone who joined a group entity (activity, hackathon project). */
case class Member(
  person: Person,
  joinedAtEpochMs: Long,
) derives JsonCodec

object Member:
  given Ordering[Member] = Ordering.by(_.joinedAtEpochMs)

/** Shared owner-and-members behaviour for activities and hackathon projects.
  *
  * Both entities currently follow the same rules: the owner is a member,
  * ownership passes to the earliest-joined remaining member, and the entity is
  * deleted when its last owner leaves. It is reasonable for them to diverge
  * later (e.g. capacity limits for projects, or activities that outlive their
  * creator). When that happens, override the specific method in the entity, or
  * stop mixing this in, rather than adding entity-specific flags here.
  */
trait Membership[Self]:
  def owner: Person
  def members: List[Member]

  protected def withMembers(members: List[Member]): Self
  def withOwner(newOwner: Person): Self

  def memberCount: Int = members.size

  def hasMember(person: Person): Boolean =
    members.exists(_.person == person)

  def isOwner(person: Person): Boolean =
    owner == person

  /** Next owner if the current owner leaves: the earliest joiner other than the owner. */
  def nextOwner: Option[Person] =
    members
      .filterNot(_.person == owner)
      .sorted
      .headOption
      .map(_.person)

  /** Would this entity be deleted if the given person left? */
  def wouldBeDeletedIfLeaves(person: Person): Boolean =
    isOwner(person) && nextOwner.isEmpty

  def withMember(person: Person, joinedAtEpochMs: Long): Self =
    if hasMember(person) then withMembers(members)
    else withMembers(members :+ Member(person, joinedAtEpochMs))

  def withoutMember(person: Person): Self =
    withMembers(members.filterNot(_.person == person))
