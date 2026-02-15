package co.wtf.openspaces

import zio.*
import zio.http.*
import zio.json.*
import zio.direct.*
import zio.http.ChannelEvent.Read

import zio.http.codec.PathCodec._
import zio.http.codec._
import zio.http.endpoint._
import zio.schema._

case class RandomActionSpawner(
  discussionService: DiscussionService,
  schedulingService: SchedulingService,
  chaosActiveRef: Ref[Boolean],
  scheduleChaosActiveRef: Ref[Boolean]):
  
  import co.wtf.openspaces.RandomActionApi._

  // Full chaos mode (votes, topics, schedules - fast)
  def isChaosActive: UIO[Boolean] = chaosActiveRef.get
  def setChaosActive(value: Boolean): UIO[Unit] = chaosActiveRef.set(value)
  def toggleChaos: UIO[Boolean] = chaosActiveRef.updateAndGet(!_)

  // Schedule-only chaos mode (moves, swaps, unschedules - slower)
  def isScheduleChaosActive: UIO[Boolean] = scheduleChaosActiveRef.get
  def setScheduleChaosActive(value: Boolean): UIO[Boolean] = scheduleChaosActiveRef.set(value).as(value)
  def toggleScheduleChaos: UIO[Boolean] = scheduleChaosActiveRef.updateAndGet(!_)

  def startSpawningRandomActions = {
    // Full chaos loop - fast (100ms)
    val chaosLoop =
      defer {
        val active = chaosActiveRef.get.run
        ZIO.when(active) {
          discussionService.randomDiscussionAction
            .debug("Random action")
            .ignore
        }.run
      }

    // Schedule chaos loop - 2 second cadence
    val scheduleChaosLoop =
      defer {
        val active = scheduleChaosActiveRef.get.run

        ZIO.when(active) {
          discussionService.randomScheduleAction
            .debug("Random Schedule action")
            .ignore
        }.run
      }

    val fullChaosFiber = chaosLoop
      .repeat(Schedule.spaced(1000.millis) && Schedule.forever)
      .debug("wilding out")
      .forkDaemon

    val scheduleChaosFiber = scheduleChaosLoop
      .repeat(Schedule.spaced(10.seconds) && Schedule.forever)
      .forkDaemon

    fullChaosFiber *> scheduleChaosFiber
  }


  val routes: Routes[Any, Response] =
    Routes(
      // Version endpoint - returns deployed commit hash
      versionGet.implement { _ =>
        val version = sys.env.getOrElse("HEROKU_SLUG_COMMIT",
                      sys.env.getOrElse("SOURCE_VERSION", "dev"))
        ZIO.succeed(VersionInfo(version))
      },

      // Full chaos endpoints
      randomActionGet.implement { _ =>
        for
          active <- isChaosActive
        yield ActiveStatus(active)
      },

      randomActionToggle.implement { _ =>
        for
          active <- toggleChaos
            .debug("Random action spawning set to")
        yield ActiveStatus(active)
      },

      randomScheduleGet.implement { _ =>
        for
          active <- isScheduleChaosActive
        yield ActiveStatus(active)
      },
      randomScheduleToggle.implement { _ =>
        for
          newState <- toggleScheduleChaos
        yield ActiveStatus(newState)
      },
      // Delete all topics
      Method.POST / "api" / "admin" / "topics" / "delete-all" -> handler {
        discussionService.deleteAllTopics
          .map(count => Response.json(DeleteTopicsResult(count).toJson))
          .orElse(ZIO.succeed(Response.status(Status.InternalServerError)))
      },

      // Auto-schedule topics
      Method.POST / "api" / "admin" / "schedule" -> handler {
        schedulingService.runScheduling
          .map(summary => Response.json(
            ScheduleResult(
              scheduled = summary.scheduled,
              moved = summary.moved,
              unscheduled = summary.unscheduled,
            ).toJson
          )
          )
          .catchAll(err =>
            ZIO.logError(s"Scheduling failed: $err") *>
            ZIO.succeed(Response.json(s"""{"error":"${err.getMessage}"}""").status(Status.InternalServerError))
          )
      },
    )

object RandomActionSpawner:
  def layer(initialActive: Boolean): ZLayer[DiscussionService & SchedulingService, Nothing, RandomActionSpawner] =
    ZLayer.fromZIO:
      for
        service <- ZIO.service[DiscussionService]
        scheduler <- ZIO.service[SchedulingService]
        chaosRef <- Ref.make(initialActive)
        scheduleChaosRef <- Ref.make(false)
      yield RandomActionSpawner(service, scheduler, chaosRef, scheduleChaosRef)
