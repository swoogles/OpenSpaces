package co.wtf.openspaces

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
import co.wtf.openspaces.hackathon.{HackathonProjectAction, HackathonProjectActionConfirmed}

sealed trait WebSocketMessage derives JsonCodec, Schema
sealed trait WebSocketMessageFromClient extends WebSocketMessage
    derives JsonCodec,
      Schema
sealed trait WebSocketMessageFromServer extends WebSocketMessage
    derives JsonCodec,
      Schema

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
