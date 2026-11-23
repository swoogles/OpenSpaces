package co.wtf.openspaces.hackathon

import co.wtf.openspaces.{ActorAction, HasActor, Person}
import neotype.*
import neotype.given
import neotype.interop.zioschema.given
import neotype.interop.ziojson.given
import zio.schema.*
import zio.json.*
import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

// Hackathon Projects (Wednesday hackday)

enum HackathonProjectAction extends ActorAction derives JsonCodec:
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
    newTitle: ProjectTitle,
    requester: Person)
  case Delete(
    projectId: HackathonProjectId,
    requester: Person)

  def actor: Person = this match
    case Create(_, creator)       => creator
    case Join(_, person)          => person
    case Leave(_, person)         => person
    case Rename(_, _, requester)  => requester
    case Delete(_, requester)     => requester
    
    
enum HackathonProjectActionConfirmed extends HasActor derives JsonCodec:
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

  def actor: Option[Person] = this match
    case Created(project) => Some(project.owner)
    case Joined(_, person, _) => Some(person)
    case Left(_, person, _) => Some(person)
    case OwnershipTransferred(_, newOwner) => Some(newOwner)
    case _ => None

  def isStateChange: Boolean = this match
    case _: Rejected | _: Unauthorized | _: StateReplace => false
    case _                                             => true
