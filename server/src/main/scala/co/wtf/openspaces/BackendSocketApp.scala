package co.wtf.openspaces

import zio.*
import zio.direct.*
import zio.http.*
import zio.http.ChannelEvent.{
  ExceptionCaught,
  Read,
  UserEvent,
  UserEventTriggered,
}
import zio.json.*

case class BackendSocketApp(
  discussionService: DiscussionService
):

  def startSpawningRandomActions(channel: WebSocketChannel) =
    ZIO
      .when(false):
        defer:
          val action =
            discussionService.randomDiscussionAction.run
          channel
            .send(
              Read(WebSocketFrame.text(action.toJson)),
            )
            .run
        .repeat(
          Schedule.spaced(
            500.millis,
            // 1.seconds,
          ) && Schedule.forever,
        )
      .forkDaemon

  val socketApp: WebSocketApp[Any] =
    Handler.webSocket { channel =>
      channel.receiveAll {
        case Read(WebSocketFrame.Text(text)) =>
          text.fromJson[WebSocketMessage] match
            case Left(value) =>
              ZIO.unit
            case Right(value) =>
              defer:
                discussionService.handleMessage(value, OpenSpacesServerChannel(channel)).run

        case UserEventTriggered(UserEvent.HandshakeComplete) =>
          ZIO.unit

        case Read(WebSocketFrame.Close(status, reason)) =>
          ZIO.unit

        case ExceptionCaught(cause) =>
          ZIO.unit

        case other =>
          ZIO.unit
      }
    }

  val socketRoutes =
    Routes(
      Method.GET / "discussions" -> handler(socketApp.toResponse),
    )

object BackendSocketApp:
  val layer =
    ZLayer.fromZIO:
      defer:
        BackendSocketApp(
          ZIO.service[DiscussionService].run
        )
