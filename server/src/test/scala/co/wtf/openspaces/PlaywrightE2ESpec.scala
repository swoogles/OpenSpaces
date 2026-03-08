package co.wtf.openspaces

import zio.*
import zio.test.*
import zio.test.Assertion.*
import com.microsoft.playwright.*

/** E2E tests using Playwright Java SDK.
  *
  * These tests require:
  * 1. E2E_TESTS=true environment variable
  * 2. Server running with TEST_MODE=true
  * 3. Playwright browsers installed: `sbt "server/Test/runMain com.microsoft.playwright.CLI install chromium"`
  *
  * Run with: `E2E_TESTS=true sbt "server/testOnly *PlaywrightE2ESpec"`
  *
  * Set BASE_URL env var to override default (http://localhost:8080)
  * Set HEADLESS=false for visible browser debugging
  */
object PlaywrightE2ESpec extends ZIOSpecDefault:

  // Skip all tests unless E2E_TESTS=true
  val e2eEnabled: Boolean = sys.env.getOrElse("E2E_TESTS", "false").toBoolean

  val baseUrl: String = sys.env.getOrElse("BASE_URL", "http://localhost:8080")
  val headless: Boolean = sys.env.getOrElse("HEADLESS", "true").toBoolean

  /** Managed Playwright resources */
  val playwrightLayer: ZLayer[Any, Throwable, Playwright] =
    ZLayer.scoped {
      ZIO.acquireRelease(
        ZIO.attemptBlocking(Playwright.create())
      )(pw => ZIO.attemptBlocking(pw.close()).orDie)
    }

  val browserLayer: ZLayer[Playwright, Throwable, Browser] =
    ZLayer.scoped {
      ZIO.serviceWithZIO[Playwright] { playwright =>
        ZIO.acquireRelease(
          ZIO.attemptBlocking {
            playwright.chromium().launch(
              new BrowserType.LaunchOptions()
                .setHeadless(headless)
                .setSlowMo(if headless then 0 else 100)
            )
          }
        )(browser => ZIO.attemptBlocking(browser.close()).orDie)
      }
    }

  /** Create a new browser context and page for each test */
  def withPage[A](test: Page => Task[A]): ZIO[Browser, Throwable, A] =
    ZIO.serviceWithZIO[Browser] { browser =>
      ZIO.scoped {
        for
          context <- ZIO.acquireRelease(
            ZIO.attemptBlocking {
              browser.newContext(
                new Browser.NewContextOptions()
                  .setViewportSize(390, 844) // iPhone 12 Pro
              )
            }
          )(ctx => ZIO.attemptBlocking(ctx.close()).orDie)
          page <- ZIO.attemptBlocking(context.newPage())
          result <- test(page)
        yield result
      }
    }

  /** Authenticate as a test user */
  def authenticate(page: Page, username: String): Task[Unit] =
    ZIO.attemptBlocking {
      page.navigate(s"$baseUrl/api/test/auth?username=$username")
    }.unit

  /** Navigate to the app and wait for it to load */
  def goToApp(page: Page): Task[Unit] =
    ZIO.attemptBlocking {
      page.navigate(baseUrl)
      page.waitForSelector(".BottomNav", new Page.WaitForSelectorOptions().setTimeout(10000))
    }.unit

  /** Navigate to Topics view */
  def goToTopics(page: Page): Task[Unit] =
    ZIO.attemptBlocking {
      page.click(".BottomNav-tab:has-text('Topics')")
      page.waitForSelector(".TopicSection-title", new Page.WaitForSelectorOptions().setTimeout(5000))
    }.unit

  /** Navigate to Activities view */
  def goToActivities(page: Page): Task[Unit] =
    ZIO.attemptBlocking {
      page.click(".BottomNav-tab:has-text('Activities')")
      page.waitForSelector(".ActivitiesView", new Page.WaitForSelectorOptions().setTimeout(5000))
    }.unit

  /** Navigate to Schedule view */
  def goToSchedule(page: Page): Task[Unit] =
    ZIO.attemptBlocking {
      page.click(".BottomNav-tab:has-text('Schedule')")
      page.waitForTimeout(1000) // Schedule view may have different selectors
    }.unit

  override def spec = suite("Playwright E2E")(
    test("authenticated user can view Topics") {
      withPage { page =>
        for
          _ <- authenticate(page, "e2e-test-topics")
          _ <- goToApp(page)
          _ <- goToTopics(page)
          visible <- ZIO.attemptBlocking(page.isVisible(".TopicSection-title"))
        yield assertTrue(visible)
      }
    },

    test("authenticated user can view Activities") {
      withPage { page =>
        for
          _ <- authenticate(page, "e2e-test-activities")
          _ <- goToApp(page)
          _ <- goToActivities(page)
          visible <- ZIO.attemptBlocking(page.isVisible(".ActivitiesView"))
        yield assertTrue(visible)
      }
    },

    test("Topics shows at most one unvoted card at a time") {
      withPage { page =>
        for
          _ <- authenticate(page, s"e2e-voting-${java.lang.System.currentTimeMillis()}")
          _ <- goToApp(page)
          _ <- goToTopics(page)
          count <- ZIO.attemptBlocking {
            // Count cards in the first TopicSection (unvoted section)
            page.locator(".TopicSection").first().locator(".TopicsContainer .TopicCard").count()
          }
        yield assertTrue(count <= 1)
      }
    },

    test("Activities shows at most one unvoted card at a time") {
      withPage { page =>
        for
          _ <- authenticate(page, s"e2e-activity-${java.lang.System.currentTimeMillis()}")
          _ <- goToApp(page)
          _ <- goToActivities(page)
          count <- ZIO.attemptBlocking {
            val pending = page.locator(".VotingQueueView-pending")
            if pending.count() > 0 then
              pending.locator(".HackathonProjectCard").count()
            else 0
          }
        yield assertTrue(count <= 1)
      }
    },

    test("nav badges show valid counts") {
      withPage { page =>
        for
          _ <- authenticate(page, "e2e-badge-test")
          _ <- goToApp(page)
          result <- ZIO.attemptBlocking {
            val topicsBadge = page.locator(".BottomNav-tab:has-text('Topics') .BottomNav-badge")
            val activitiesBadge = page.locator(".BottomNav-tab:has-text('Activities') .BottomNav-badge")

            // Badges are optional, but if present should be numeric
            val topicsOk = topicsBadge.count() == 0 ||
              topicsBadge.textContent().matches("\\d+\\+?")
            val activitiesOk = activitiesBadge.count() == 0 ||
              activitiesBadge.textContent().matches("\\d+\\+?")

            topicsOk && activitiesOk
          }
        yield assertTrue(result)
      }
    },

    test("Schedule view loads") {
      withPage { page =>
        for
          _ <- authenticate(page, "e2e-schedule-test")
          _ <- goToApp(page)
          _ <- goToSchedule(page)
          visible <- ZIO.attemptBlocking {
            page.isVisible(".ScheduleView") || page.isVisible(".CalendarDayView")
          }
        yield assertTrue(visible)
      }
    },

    test("create button opens sheet") {
      withPage { page =>
        for
          _ <- authenticate(page, "e2e-create-test")
          _ <- goToApp(page)
          _ <- ZIO.attemptBlocking {
            page.click(".BottomNav-tab--create")
            page.waitForSelector(".CreateSheet", new Page.WaitForSelectorOptions().setTimeout(3000))
          }
          visible <- ZIO.attemptBlocking(page.isVisible(".CreateSheet"))
        yield assertTrue(visible)
      }
    },
  ).provide(
    playwrightLayer,
    browserLayer,
  ) @@ TestAspect.sequential @@ TestAspect.withLiveClock @@ TestAspect.tag("e2e") @@
    (if e2eEnabled then TestAspect.identity else TestAspect.ignore)
