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
          println("Received message: " + text)
          text.fromJson[WebSocketMessage] match
            case Left(value) =>
              ZIO.debug(s"Server received invalid message: $value")
            case Right(value) =>
              defer:
                discussionService.handleMessage(value, OpenSpacesServerChannel(channel)).debug("handleMessage").run
                // This can't be done here, it can only be done after we've recognized the value as a ticket down in discussion service.
                // startSpawningRandomActions(channel).debug("startSpawningRandomActions").run

        case UserEventTriggered(UserEvent.HandshakeComplete) =>
          ZIO.debug("Server Handshake complete. Waiting for a valid ticket before sending data.")

        case Read(WebSocketFrame.Close(status, reason)) =>
          Console.printLine(
            "Closing channel with status: " + status + " and reason: " + reason,
          )

        case ExceptionCaught(cause) =>
          Console.printLine(s"Channel error!: ${cause.getMessage}")

        case other =>
          ZIO.debug("Other channel event: " + other)
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
