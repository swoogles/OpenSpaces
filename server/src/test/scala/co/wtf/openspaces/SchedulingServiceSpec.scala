package co.wtf.openspaces

import co.wtf.openspaces.discussions.SchedulingService
import zio.durationInt
import zio.test.*

import java.time.{Instant, LocalDateTime}

object SchedulingServiceSpec extends ZIOSpecDefault:

  private def slotAt(startTime: LocalDateTime): TimeSlot =
    TimeSlot(startTime, startTime.plusMinutes(50))

  override def spec =
    suite("SchedulingServiceSpec")(
      test("occupied slots freeze within 15 minutes of start") {
        val now = LocalDateTime.parse("2026-03-03T08:50:00")

        assertTrue(
          SchedulingService.shouldFreezeOccupiedSlot(now, slotAt(LocalDateTime.parse("2026-03-03T09:04:00"))),
          !SchedulingService.shouldFreezeOccupiedSlot(now, slotAt(LocalDateTime.parse("2026-03-03T09:05:00"))),
        )
      },
      test("empty slots remain assignable right until the start time") {
        val now = LocalDateTime.parse("2026-03-03T08:50:00")

        assertTrue(
          SchedulingService.canAssignIntoSlot(now, slotAt(LocalDateTime.parse("2026-03-03T08:51:00"))),
          !SchedulingService.canAssignIntoSlot(now, slotAt(LocalDateTime.parse("2026-03-03T08:50:00"))),
          !SchedulingService.canAssignIntoSlot(now, slotAt(LocalDateTime.parse("2026-03-03T08:49:00"))),
        )
      },
      test("auto-scheduling delay aligns to the 30-minute cadence anchored at 2026-03-03 09:00 MST") {
        val beforeAnchor = Instant.parse("2026-03-03T15:45:00Z")
        val afterAnchor = Instant.parse("2026-03-03T16:07:00Z")
        val onBoundary = Instant.parse("2026-03-03T16:30:00Z")

        assertTrue(
          SchedulingService.nextAutoSchedulingDelay(beforeAnchor) == 15.minutes,
          SchedulingService.nextAutoSchedulingDelay(afterAnchor) == 23.minutes,
          SchedulingService.nextAutoSchedulingDelay(onBoundary) == zio.Duration.Zero,
        )
      },
    )
