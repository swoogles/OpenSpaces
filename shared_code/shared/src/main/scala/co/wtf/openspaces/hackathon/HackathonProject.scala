package co.wtf.openspaces.hackathon

import neotype.*
import neotype.interop.ziojson.given
import zio.json.*
import co.wtf.openspaces.{Member, Membership, Person}

type HackathonProjectId = HackathonProjectId.Type
object HackathonProjectId extends Newtype[Long]

type ProjectTitle = ProjectTitle.Type
object ProjectTitle extends Newtype[String]:
  override inline def validate(value: String) =
    if value.trim.length < 3 then "Title must be at least 3 characters"
    else if value.trim.length > 80 then "Title must be under 80 characters"
    else true

case class HackathonProject(
  id: HackathonProjectId,
  title: ProjectTitle,
  owner: Person,
  members: List[Member],
  createdAtEpochMs: Long,
  ownerDisplayName: Option[String] = None,
  slackThreadUrl: Option[String] = None,
) extends Membership[HackathonProject] derives JsonCodec:

  protected def withMembers(members: List[Member]): HackathonProject =
    copy(members = members)

  def withOwner(newOwner: Person): HackathonProject =
    copy(owner = newOwner)

  def isLargeGroup: Boolean = memberCount > 5

  def ownerName: String = ownerDisplayName.getOrElse(owner.unwrap)

  def titleText: String = title.unwrap

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
      members = List(Member(owner, createdAtEpochMs)),
      createdAtEpochMs = createdAtEpochMs,
      ownerDisplayName = ownerDisplayName,
      slackThreadUrl = None,
    )
