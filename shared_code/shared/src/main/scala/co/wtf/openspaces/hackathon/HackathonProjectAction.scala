package co.wtf.openspaces.hackathon

import co.wtf.openspaces.DiscussionAction.Rename
import co.wtf.openspaces.VotePosition.Interested
import neotype.*
import neotype.given
import neotype.interop.zioschema.given
import neotype.interop.ziojson.given
import zio.schema.*
import zio.json.*
import java.time.{LocalDate, LocalDateTime}
import java.util.UUID
import co.wtf.openspaces.hackathon.{HackathonProject, HackathonProjectId, ProjectTitle}
import co.wtf.openspaces.Person

// Hackathon Projects (Wednesday hackday)

enum HackathonProjectAction derives JsonCodec:
  case Create(
    title: ProjectTitle,
    creator: Person)
  case Join(
    projectId: HackathonProjectId,
    person: Person)
  case Leave(
    projectId: HackathonProjectId,
    person: Person)
  case Rename(
    projectId: HackathonProjectId,
    newTitle: ProjectTitle)
  case Delete(
    projectId: HackathonProjectId,
    requester: Person)
    
    
enum HackathonProjectActionConfirmed derives JsonCodec:
  case Created(
    project: HackathonProject)
  case Joined(
    projectId: HackathonProjectId,
    person: Person,
    joinedAtEpochMs: Long)
  case Left(
    projectId: HackathonProjectId,
    person: Person,
    newOwner: Option[Person])
  case OwnershipTransferred(
    projectId: HackathonProjectId,
    newOwner: Person)
  case Renamed(
    projectId: HackathonProjectId,
    newTitle: ProjectTitle)
  case Deleted(
    projectId: HackathonProjectId)
  case SlackThreadLinked(
    projectId: HackathonProjectId,
    slackThreadUrl: String)
  case StateReplace(
    projects: List[HackathonProject])
  case Unauthorized(
    action: HackathonProjectAction)
  case Rejected(
    action: HackathonProjectAction)
