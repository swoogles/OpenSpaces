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
sealed trait WebSocketMessageFromClient extends WebSocketMessage
    derives JsonCodec
sealed trait WebSocketMessageFromServer extends WebSocketMessage
    derives JsonCodec

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

case class Ticket(
  uuid: UUID)
    extends WebSocketMessageFromClient
    derives JsonCodec,
      Schema

case class DiscussionActionMessage(
  action: DiscussionAction)
    extends WebSocketMessageFromClient

case class DiscussionActionConfirmedMessage(
  event: DiscussionActionConfirmed)
    extends WebSocketMessageFromServer

case class LightningTalkActionMessage(
  action: LightningTalkAction)
    extends WebSocketMessageFromClient

case class LightningTalkActionConfirmedMessage(
  event: LightningTalkActionConfirmed)
    extends WebSocketMessageFromServer

case class HackathonProjectActionMessage(
  action: HackathonProjectAction)
    extends WebSocketMessageFromClient

case class HackathonProjectActionConfirmedMessage(
  event: HackathonProjectActionConfirmed)
    extends WebSocketMessageFromServer

case class ActivityActionMessage(
  action: ActivityAction)
    extends WebSocketMessageFromClient

case class ActivityActionConfirmedMessage(
  event: ActivityActionConfirmed)
    extends WebSocketMessageFromServer

// Authorization messages

/** Sent from server to client on connect to tell them their authorization status */
case class AuthorizationStatusMessage(
  status: AuthorizationStatus)
    extends WebSocketMessageFromServer

/** Sent from client (admin) to server to manage user authorization */
case class AuthorizationActionMessage(
  action: AuthorizationAction)
    extends WebSocketMessageFromClient

/** Broadcast from server when authorization changes */
case class AuthorizationActionConfirmedMessage(
  event: AuthorizationActionConfirmed)
    extends WebSocketMessageFromServer

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

// Location sharing messages

/** Sent from client to server to manage location sharing */
case class LocationActionMessage(
  action: LocationAction)
    extends WebSocketMessageFromClient

/** Broadcast from server when location state changes */
case class LocationActionConfirmedMessage(
  event: LocationActionConfirmed)
    extends WebSocketMessageFromServer

// Slack reply counts

/** Lightweight heartbeat frame to keep websocket connections from going idle */
case object KeepAliveMessage
    extends WebSocketMessageFromServer

/** Broadcast from server with updated Slack thread reply counts */
case class SlackReplyCountsMessage(
  counts: SlackReplyCounts)
    extends WebSocketMessageFromServer

/** Reply counts for all entity types, keyed by entity ID (as String for JSON compatibility) */
case class SlackReplyCounts(
  discussions: Map[String, Int],
  lightningTalks: Map[String, Int],
  hackathonProjects: Map[String, Int],
  activities: Map[String, Int],
) derives JsonCodec
