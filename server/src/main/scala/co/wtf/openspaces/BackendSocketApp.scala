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
  discussionService: SessionService,
  randomActionSpawner: RandomActionSpawner):

  val socketApp: WebSocketApp[Any] =
    Handler.webSocket { channel =>
      defer:
        val openSpacesChannel = OpenSpacesServerChannel(channel)
        channel.receiveAll {
          case Read(WebSocketFrame.Text(text)) =>
            text.fromJson[WebSocketMessageFromClient] match
              case Left(value) =>
                ZIO.unit
              case Right(value) =>
                defer:
                  discussionService
                    .handleMessage(value,
                                   openSpacesChannel,
                    )
                    .run

          case UserEventTriggered(UserEvent.HandshakeComplete) =>
            ZIO.unit

          case Read(WebSocketFrame.Close(status, reason)) =>
            discussionService.removeChannel(openSpacesChannel)

          case ExceptionCaught(cause) =>
            discussionService.removeChannel(openSpacesChannel)

          case other =>
            ZIO.unit
        }.run
    }

  val socketRoutes =
    Routes(
      Method.GET / "discussions" -> handler(socketApp.toResponse),
    )

object BackendSocketApp:
  val layer =
    ZLayer.derive[BackendSocketApp]
