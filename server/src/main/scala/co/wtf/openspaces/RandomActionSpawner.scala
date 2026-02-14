package co.wtf.openspaces

import zio.*
import zio.http.*
import zio.json.*
import zio.direct.*
import zio.http.ChannelEvent.Read

import zio.http.codec.PathCodec._
import zio.http.codec._
import zio.http.endpoint._
import zio.http.endpoint.openapi._
import zio.schema._

case class ActiveStatus(active: Boolean) derives Schema

case class RandomActionSpawner(
  discussionService: DiscussionService,
  schedulingService: SchedulingService,
  chaosActiveRef: Ref[Boolean],
  scheduleChaosActiveRef: Ref[Boolean]):

  // Full chaos mode (votes, topics, schedules - fast)
  def isChaosActive: UIO[Boolean] = chaosActiveRef.get
  def setChaosActive(value: Boolean): UIO[Unit] = chaosActiveRef.set(value)
  def toggleChaos: UIO[Boolean] = chaosActiveRef.updateAndGet(!_)

  // Schedule-only chaos mode (moves, swaps, unschedules - slower)
  def isScheduleChaosActive: UIO[Boolean] = scheduleChaosActiveRef.get
  def setScheduleChaosActive(value: Boolean): UIO[Unit] = scheduleChaosActiveRef.set(value)
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
        }
      }
    
    // Schedule chaos loop - 2 second cadence
    val scheduleChaosLoop = 
      defer {
        val active = scheduleChaosActiveRef.get.run
      
        ZIO.when(active) {
          discussionService.randomScheduleAction
            .debug("Random Schedule action")
            .ignore
        }
      }
    
    val fullChaosFiber = chaosLoop
      .repeat(Schedule.spaced(100.millis) && Schedule.forever)
      .forkDaemon
      
    val scheduleChaosFiber = scheduleChaosLoop
      .repeat(Schedule.spaced(2.seconds) && Schedule.forever)
      .forkDaemon
    
    fullChaosFiber *> scheduleChaosFiber
    }

    
  val randomActionGet =
    Endpoint(RoutePattern.GET / "api" / "admin" / "random-actions")
    //  PathCodec.int("id")
    .out[ActiveStatus]
    
  val randomActionToggle =
    Endpoint(RoutePattern.POST / "api" / "admin" / "random-actions" / "toggle")
      .out[ActiveStatus]
      
  val randomActionStart =
    Endpoint(RoutePattern.POST / "api" / "admin" / "random-actions" / "start")
      .out[ActiveStatus]
      
  val randomActionStop =
    Endpoint(RoutePattern.POST / "api" / "admin" / "random-actions" / "stop")
      .out[ActiveStatus]
    
  val routes: Routes[Any, Response] =
    Routes(
      // Version endpoint - returns deployed commit hash
      Method.GET / "api" / "version" -> handler {
        val version = sys.env.getOrElse("HEROKU_SLUG_COMMIT", 
                      sys.env.getOrElse("SOURCE_VERSION", "dev"))
        ZIO.succeed(Response.json(s"""{"version":"$version"}"""))
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
        yield ActiveStatus(active)
      },
      
      randomActionStart.implement { _ =>
        for
          _ <- setChaosActive(true)
        yield ActiveStatus(true)
      },
      
      randomActionStop.implement { _ =>
        for
          _ <- setChaosActive(false)
        yield ActiveStatus(false)
      },
      // Schedule chaos endpoints
      Method.GET / "api" / "admin" / "schedule-chaos" -> handler {
        for
          active <- isScheduleChaosActive
        yield Response.json(s"""{"active":$active}""")
      },
      Method.POST / "api" / "admin" / "schedule-chaos" / "toggle" -> handler {
        for
          newState <- toggleScheduleChaos
        yield Response.json(s"""{"active":$newState}""")
      },
      Method.POST / "api" / "admin" / "schedule-chaos" / "start" -> handler {
        for
          _ <- setScheduleChaosActive(true)
        yield Response.json("""{"active":true}""")
      },
      Method.POST / "api" / "admin" / "schedule-chaos" / "stop" -> handler {
        for
          _ <- setScheduleChaosActive(false)
        yield Response.json("""{"active":false}""")
      },
      
      // Delete all topics
      Method.POST / "api" / "admin" / "topics" / "delete-all" -> handler {
        discussionService.deleteAllTopics
          .map(count => Response.json(s"""{"deleted":$count}"""))
          .orElse(ZIO.succeed(Response.status(Status.InternalServerError)))
      },
      
      // Auto-schedule topics
      Method.POST / "api" / "admin" / "schedule" -> handler {
        schedulingService.runScheduling
          .map(summary => Response.json(
            s"""{"scheduled":${summary.scheduled},"moved":${summary.moved},"unscheduled":${summary.unscheduled}}"""
          ))
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
