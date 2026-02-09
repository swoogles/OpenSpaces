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
  discussionService: DiscussionService,
  active: Boolean):
  def startSpawningRandomActions(
    channel: WebSocketChannel,
  ) =
    ZIO
      .when(active):
        defer:
          val action =
            discussionService.randomDiscussionAction
            .debug("Random action")
            .run
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
  def layer(
    active: Boolean,
  ) =
    ZLayer.fromZIO:
      defer:
        RandomActionSpawner(
          ZIO.service[DiscussionService].run,
          active,
        )
