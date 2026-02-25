package co.wtf.openspaces

import co.wtf.openspaces.discussions.{DiscussionAction, DiscussionActionConfirmed}
import co.wtf.openspaces.hackathon.{HackathonProjectAction, HackathonProjectActionConfirmed}
import co.wtf.openspaces.lighting_talks.{LightningTalkAction, LightningTalkActionConfirmed}
import co.wtf.openspaces.activities.{ActivityAction, ActivityActionConfirmed}
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
    derives JsonCodec

case class DiscussionActionConfirmedMessage(
  event: DiscussionActionConfirmed)
    extends WebSocketMessageFromServer
    derives JsonCodec

case class LightningTalkActionMessage(
  action: LightningTalkAction)
    extends WebSocketMessageFromClient
    derives JsonCodec

case class LightningTalkActionConfirmedMessage(
  event: LightningTalkActionConfirmed)
    extends WebSocketMessageFromServer
    derives JsonCodec

case class HackathonProjectActionMessage(
  action: HackathonProjectAction)
    extends WebSocketMessageFromClient
    derives JsonCodec

case class HackathonProjectActionConfirmedMessage(
  event: HackathonProjectActionConfirmed)
    extends WebSocketMessageFromServer
    derives JsonCodec

case class ActivityActionMessage(
  action: ActivityAction)
    extends WebSocketMessageFromClient
    derives JsonCodec

case class ActivityActionConfirmedMessage(
  event: ActivityActionConfirmed)
    extends WebSocketMessageFromServer
    derives JsonCodec
