package co.wtf.openspaces

import zio.*
import zio.http.*
import zio.json.*
import zio.direct.*
import zio.http.ChannelEvent.{
  ExceptionCaught,
  Read,
  UserEvent,
  UserEventTriggered,
}

case class RandomActionSpawner(
  discussionService: DiscussionService):
  def startSpawningRandomActions(
    channel: WebSocketChannel,
  ) =
    ZIO
      .when(true):
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

object RandomActionSpawner:
  val layer =
    ZLayer.fromZIO:
      defer:
        RandomActionSpawner(
          ZIO.service[DiscussionService].run,
        )
