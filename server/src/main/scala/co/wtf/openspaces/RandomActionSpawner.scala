package co.wtf.openspaces

import co.wtf.openspaces.db.ConfirmedActionRepository
import co.wtf.openspaces.discussions.SchedulingService
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
  discussionService: SessionService,
  schedulingService: SchedulingService,
  confirmedActionRepository: ConfirmedActionRepository,
  chaosActiveRef: Ref[Boolean],
  scheduleChaosActiveRef: Ref[Boolean],
  hackathonChaosActiveRef: Ref[Boolean]):
  
  import co.wtf.openspaces.RandomActionApi._

  // Full chaos mode (votes, topics, schedules - fast)
  def isChaosActive: UIO[Boolean] = chaosActiveRef.get
  def setChaosActive(value: Boolean): UIO[Unit] = chaosActiveRef.set(value)
  def toggleChaos: UIO[Boolean] = chaosActiveRef.updateAndGet(!_)

  // Schedule-only chaos mode (moves, swaps, unschedules - slower)
  def isScheduleChaosActive: UIO[Boolean] = scheduleChaosActiveRef.get
  def setScheduleChaosActive(value: Boolean): UIO[Boolean] = scheduleChaosActiveRef.set(value).as(value)
  def toggleScheduleChaos: UIO[Boolean] = scheduleChaosActiveRef.updateAndGet(!_)

  // Hackathon chaos mode (create, join, leave, delete projects)
  def isHackathonChaosActive: UIO[Boolean] = hackathonChaosActiveRef.get
  def setHackathonChaosActive(value: Boolean): UIO[Boolean] = hackathonChaosActiveRef.set(value).as(value)
  def toggleHackathonChaos: UIO[Boolean] = hackathonChaosActiveRef.updateAndGet(!_)

  def startSpawningRandomActions = {
    // Full chaos loop - fast (100ms)
    val chaosLoop =
      defer {
        val active = chaosActiveRef.get.run
        val lightningChancePercent = 20
        val actionType = Random.nextIntBounded(100).run
        if !active then
          ZIO.unit.run
        else if actionType < lightningChancePercent then
          discussionService.randomLightningAction
            .debug("Random lightning action")
            .ignore
            .run
        else
          discussionService.randomDiscussionAction
            .debug("Random action")
            .ignore
            .run
      }

    // Schedule chaos loop - 10 second cadence
    val scheduleChaosLoop =
      defer {
        val active = scheduleChaosActiveRef.get.run

        ZIO.when(active) {
          discussionService.randomScheduleAction
            .debug("Random Schedule action")
            .ignore
        }.run
      }

    // Hackathon chaos loop - 3 second cadence
    val hackathonChaosLoop =
      defer {
        val active = hackathonChaosActiveRef.get.run

        ZIO.when(active) {
          discussionService.randomHackathonAction
            .debug("Random Hackathon action")
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

    val hackathonChaosFiber = hackathonChaosLoop
      .repeat(Schedule.spaced(3.seconds) && Schedule.forever)
      .forkDaemon

    fullChaosFiber *> scheduleChaosFiber *> hackathonChaosFiber
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
      RandomActionApi.deleteAllTopics.implement { _ =>
        discussionService.deleteAllTopics
          .orDie
          .map(DeleteTopicsResult(_))
      },

      RandomActionApi.deleteAllRandomUsersRecords.implement { _ =>
        discussionService.deleteAllRandomUserRecords
          .orDie
      },

      // Auto-schedule topics
      RandomActionApi.runScheduling.implement { _ =>
        schedulingService.runScheduling
          .orDie
          .map { summary =>
            ScheduleResult(
              scheduled = summary.scheduled,
              moved = summary.moved,
              unscheduled = summary.unscheduled,
              lockedExcluded = summary.lockedExcluded,
            )
          }
      },

      // Hackathon chaos endpoints
      RandomActionApi.hackathonChaosGet.implement { _ =>
        for
          active <- isHackathonChaosActive
        yield ActiveStatus(active)
      },
      RandomActionApi.hackathonChaosToggle.implement { _ =>
        for
          newState <- toggleHackathonChaos
        yield ActiveStatus(newState)
      },

      // Confirmed actions log
      RandomActionApi.confirmedActionsGet.implement { _ =>
        confirmedActionRepository.findAll
          .map(rows =>
            ConfirmedActionsResponse(
              rows.map(row =>
                ConfirmedActionEntry(
                  id = row.id,
                  createdAtEpochMs = row.createdAt.toInstant.toEpochMilli,
                  entityType = row.entityType,
                  actionType = row.actionType,
                  payload = row.payload,
                  actor = row.actor,
                )
              ).toList
            )
          )
          .orDie
      },
    )

object RandomActionSpawner:
  def layer(initialActive: Boolean): ZLayer[SessionService & SchedulingService & ConfirmedActionRepository, Nothing, RandomActionSpawner] =
    ZLayer.fromZIO:
      for
        service <- ZIO.service[SessionService]
        scheduler <- ZIO.service[SchedulingService]
        confirmedActionRepo <- ZIO.service[ConfirmedActionRepository]
        chaosRef <- Ref.make(initialActive)
        scheduleChaosRef <- Ref.make(false)
        hackathonChaosRef <- Ref.make(false)
      yield RandomActionSpawner(service, scheduler, confirmedActionRepo, chaosRef, scheduleChaosRef, hackathonChaosRef)
