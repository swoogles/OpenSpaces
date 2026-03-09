package co.wtf.openspaces

import co.wtf.openspaces.discussions.{DiscussionAction, DiscussionActionConfirmed}
import co.wtf.openspaces.hackathon.{HackathonProjectAction, HackathonProjectActionConfirmed}
import co.wtf.openspaces.lighting_talks.{LightningTalkAction, LightningTalkActionConfirmed}
import co.wtf.openspaces.activities.{ActivityAction, ActivityActionConfirmed}
import co.wtf.openspaces.location.{LocationAction, LocationActionConfirmed}
import neotype.*
import neotype.given
import neotype.interop.zioschema.given
import neotype.interop.ziojson.given
import zio.schema.*
import zio.json.*

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

sealed trait WebSocketMessage derives JsonCodec

enum WebSocketMessageFromClient extends WebSocketMessage derives JsonCodec:
  case Ticket(uuid: UUID)
  case DiscussionActionMessage(action: DiscussionAction)
  case LightningTalkActionMessage(action: LightningTalkAction)
  case HackathonProjectActionMessage(action: HackathonProjectAction)
  case ActivityActionMessage(action: ActivityAction)
  case AuthorizationActionMessage(action: AuthorizationAction)
  case LocationActionMessage(action: LocationAction)

enum WebSocketMessageFromServer extends WebSocketMessage derives JsonCodec:
  case DiscussionActionConfirmedMessage(event: DiscussionActionConfirmed)
  case LightningTalkActionConfirmedMessage(event: LightningTalkActionConfirmed)
  case HackathonProjectActionConfirmedMessage(event: HackathonProjectActionConfirmed)
  case ActivityActionConfirmedMessage(event: ActivityActionConfirmed)
  case AuthorizationStatusMessage(status: AuthorizationStatus)
  case AuthorizationActionConfirmedMessage(event: AuthorizationActionConfirmed)
  case LocationActionConfirmedMessage(event: LocationActionConfirmed)
  case KeepAliveMessage
  case SlackReplyCountsMessage(counts: SlackReplyCounts)

given Schema[WebSocketMessageFromClient] =
  Schema[String].transformOrFail(
    raw => raw.fromJson[WebSocketMessageFromClient],
    message => Right(message.toJson),
  )

given Schema[WebSocketMessageFromServer] =
  Schema[String].transformOrFail(
    raw => raw.fromJson[WebSocketMessageFromServer],
    message => Right(message.toJson),
  )

given Schema[WebSocketMessageFromClient.Ticket] = DeriveSchema.gen
given JsonCodec[WebSocketMessageFromClient.Ticket] = DeriveJsonCodec.gen

// Authorization domain types

/** User's current authorization status */
case class AuthorizationStatus(
  username: String,
  approved: Boolean,
  isAdmin: Boolean,
  pendingUsers: Option[List[PendingUser]] = None, // Only populated for admins
  approvedUsers: Option[List[ApprovedUser]] = None, // Only populated for admins
) derives JsonCodec

case class PendingUser(
  username: String,
  displayName: Option[String],
  requestedAt: String, // ISO datetime string
) derives JsonCodec

case class ApprovedUser(
  username: String,
  displayName: Option[String],
) derives JsonCodec

/** Admin actions for user authorization */
enum AuthorizationAction derives JsonCodec:
  case ApproveUser(username: String)
  case RevokeUser(username: String)
  case RefreshUserList // Request updated pending/approved lists

/** Confirmed authorization actions broadcast to clients */
enum AuthorizationActionConfirmed derives JsonCodec:
  case UserApproved(username: String)
  case UserRevoked(username: String)
  case Unauthorized(action: AuthorizationAction)
  case Rejected(action: AuthorizationAction, reason: String)
  case UserListRefreshed(pendingUsers: List[PendingUser], approvedUsers: List[ApprovedUser])

/** Reply counts for all entity types, keyed by entity ID (as String for JSON compatibility) */
case class SlackReplyCounts(
  discussions: Map[String, Int],
  lightningTalks: Map[String, Int],
  hackathonProjects: Map[String, Int],
  activities: Map[String, Int],
) derives JsonCodec
