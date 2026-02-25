package co.wtf.openspaces.hackathon

import neotype.*
import neotype.interop.ziojson.given
import zio.json.*
import co.wtf.openspaces.Person

type HackathonProjectId = HackathonProjectId.Type
object HackathonProjectId extends Newtype[Long]

type ProjectTitle = ProjectTitle.Type
object ProjectTitle extends Newtype[String]:
  override inline def validate(value: String) =
    if value.trim.length < 3 then "Title must be at least 3 characters"
    else if value.trim.length > 80 then "Title must be under 80 characters"
    else true

case class ProjectMember(
  person: Person,
  joinedAtEpochMs: Long,
) derives JsonCodec

object ProjectMember:
  given Ordering[ProjectMember] = Ordering.by(_.joinedAtEpochMs)

case class HackathonProject(
  id: HackathonProjectId,
  title: ProjectTitle,
  owner: Person,
  members: List[ProjectMember],
  createdAtEpochMs: Long,
  ownerDisplayName: Option[String] = None,
  slackThreadUrl: Option[String] = None,
) derives JsonCodec:

  def memberCount: Int = members.size

  def isLargeGroup: Boolean = memberCount > 5

  def ownerName: String = ownerDisplayName.getOrElse(owner.unwrap)

  def titleText: String = title.unwrap

  def hasMember(person: Person): Boolean =
    members.exists(_.person == person)

  def isOwner(person: Person): Boolean =
    owner == person

  /** Next owner if current owner leaves (earliest joiner excluding owner) */
  def nextOwner: Option[Person] =
    members
      .filterNot(_.person == owner)
      .sorted
      .headOption
      .map(_.person)

  /** Would this project be deleted if the given person leaves? */
  def wouldBeDeletedIfLeaves(person: Person): Boolean =
    isOwner(person) && nextOwner.isEmpty

  def withMember(person: Person, joinedAtEpochMs: Long): HackathonProject =
    if hasMember(person) then this
    else copy(members = members :+ ProjectMember(person, joinedAtEpochMs))

  def withoutMember(person: Person): HackathonProject =
    copy(members = members.filterNot(_.person == person))

  def withOwner(newOwner: Person): HackathonProject =
    copy(owner = newOwner)

object HackathonProject:
  def create(
    id: HackathonProjectId,
    title: ProjectTitle,
    owner: Person,
    ownerDisplayName: Option[String] = None,
    createdAtEpochMs: Long = java.lang.System.currentTimeMillis(),
  ): HackathonProject =
    HackathonProject(
      id = id,
      title = title,
      owner = owner,
      members = List(ProjectMember(owner, createdAtEpochMs)),
      createdAtEpochMs = createdAtEpochMs,
      ownerDisplayName = ownerDisplayName,
      slackThreadUrl = None,
    )
