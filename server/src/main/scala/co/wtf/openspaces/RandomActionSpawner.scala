package co.wtf.openspaces

import zio.*
import zio.http.*
import zio.json.*
import zio.direct.*
import zio.http.ChannelEvent.Read

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

  def startSpawningRandomActions =
    // Full chaos loop - fast (100ms)
    val chaosLoop = chaosActiveRef.get.flatMap { active =>
      if active then
        discussionService.randomDiscussionAction
          .debug("Random action")
          .ignore
      else
        ZIO.unit
    }
    
    // Schedule chaos loop - 2 second cadence
    val scheduleChaosLoop = scheduleChaosActiveRef.get.flatMap { active =>
      if active then
        discussionService.randomScheduleAction
          .debug("Random schedule action")
          .ignore
      else
        ZIO.unit
    }
    
    val fullChaosFiber = chaosLoop
      .repeat(Schedule.spaced(100.millis) && Schedule.forever)
      .forkDaemon
      
    val scheduleChaosFiber = scheduleChaosLoop
      .repeat(Schedule.spaced(2.seconds) && Schedule.forever)
      .forkDaemon
    
    fullChaosFiber *> scheduleChaosFiber

  val routes: Routes[Any, Response] =
    Routes(
      // Full chaos endpoints
      Method.GET / "api" / "admin" / "random-actions" -> handler {
        for
          active <- isChaosActive
        yield Response.json(s"""{"active":$active}""")
      },
      Method.POST / "api" / "admin" / "random-actions" / "toggle" -> handler {
        for
          newState <- toggleChaos
        yield Response.json(s"""{"active":$newState}""")
      },
      Method.POST / "api" / "admin" / "random-actions" / "start" -> handler {
        for
          _ <- setChaosActive(true)
        yield Response.json("""{"active":true}""")
      },
      Method.POST / "api" / "admin" / "random-actions" / "stop" -> handler {
        for
          _ <- setChaosActive(false)
        yield Response.json("""{"active":false}""")
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
