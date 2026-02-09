package co.wtf.openspaces

import zio.*
import zio.http.*
import zio.json.*
import zio.direct.*
import zio.http.ChannelEvent.Read

case class RandomActionSpawner(
  discussionService: DiscussionService,
  activeRef: Ref[Boolean]):

  def isActive: UIO[Boolean] = activeRef.get

  def setActive(value: Boolean): UIO[Unit] = activeRef.set(value)

  def toggle: UIO[Boolean] = activeRef.updateAndGet(!_)

  def startSpawningRandomActions =
    val loop = activeRef.get.flatMap { active =>
      if active then
        discussionService.randomDiscussionAction
          .debug("Random action")
          .ignore
      else
        ZIO.unit
    }
    loop
      .repeat(Schedule.spaced(500.millis) && Schedule.forever)
      .forkDaemon

  val routes: Routes[Any, Response] =
    Routes(
      Method.GET / "api" / "admin" / "random-actions" -> handler {
        for
          active <- isActive
        yield Response.json(s"""{"active":$active}""")
      },
      Method.POST / "api" / "admin" / "random-actions" / "toggle" -> handler {
        for
          newState <- toggle
        yield Response.json(s"""{"active":$newState}""")
      },
      Method.POST / "api" / "admin" / "random-actions" / "start" -> handler {
        for
          _ <- setActive(true)
        yield Response.json("""{"active":true}""")
      },
      Method.POST / "api" / "admin" / "random-actions" / "stop" -> handler {
        for
          _ <- setActive(false)
        yield Response.json("""{"active":false}""")
      },
      Method.POST / "api" / "admin" / "topics" / "delete-all" -> handler {
        discussionService.deleteAllTopics
          .map(count => Response.json(s"""{"deleted":$count}"""))
          .orElse(ZIO.succeed(Response.status(Status.InternalServerError)))
      },
    )

object RandomActionSpawner:
  def layer(initialActive: Boolean): ZLayer[DiscussionService, Nothing, RandomActionSpawner] =
    ZLayer.fromZIO:
      for
        service <- ZIO.service[DiscussionService]
        ref <- Ref.make(initialActive)
      yield RandomActionSpawner(service, ref)
