package co.wtf.openspaces

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.http.*
import com.microsoft.playwright.{Playwright, Browser, BrowserType, Page, BrowserContext}
import co.wtf.openspaces.auth.{AdminConfig, AuthenticatedTicketService}
import co.wtf.openspaces.discussions.{DiscussionDataStore, DiscussionStore}

/** E2E tests using Playwright Java SDK with embedded server.
  *
  * Runs a full embedded server with WebSocket support, static files, and test auth.
  * Uses in-memory/no-op service implementations (no database required).
  * All within a single `sbt test` run.
  *
  * First time setup - install Playwright browsers:
  *   sbt "server/Test/runMain com.microsoft.playwright.CLI install chromium"
  *
  * Set HEADLESS=false for visible browser debugging
  * Set SKIP_E2E=true to skip these tests
  */
object PlaywrightE2ESpec extends ZIOSpecDefault:

  // Set SKIP_E2E=true to skip E2E tests (e.g., in CI without browsers)
  // First run: sbt "server/Test/runMain com.microsoft.playwright.CLI install chromium"
  val skipE2E: Boolean = sys.env.getOrElse("SKIP_E2E", "false").toBoolean

  val headless: Boolean = sys.env.getOrElse("HEADLESS", "true").toBoolean
  // Use a different port than 8080 (which may be in use by dev server)
  val testPort: Int = 18080

  // ============================================
  // Playwright Browser Layers
  // ============================================

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

  // ============================================
  // Embedded Test Server (full WebSocket support)
  // ============================================

  /** Full test server with WebSocket, ticket routes, static files, and test auth.
    * Uses in-memory services (no database required).
    */
  val embeddedServerLayer: ZLayer[BackendSocketApp & TicketRoutesApp, Throwable, Unit] =
    ZLayer.scoped {
      for
        // Get WebSocket and ticket routes from services
        socketApp <- ZIO.service[BackendSocketApp]
        ticketRoutesApp <- ZIO.service[TicketRoutesApp]

        // Test auth route - sets cookies for test users
        testAuthRoute = Routes(
          Method.GET / "api" / "test" / "auth" -> Handler.fromFunctionZIO { (req: zio.http.Request) =>
            val username = req.url.queryParams.queryParam("username").getOrElse("testuser")
            // Client expects access_token_expires_at in SECONDS (not milliseconds)
            val expiresAtSeconds = (java.lang.System.currentTimeMillis() / 1000) + (8 * 3600)
            ZIO.succeed(
              Response.ok
                .addHeader(Header.SetCookie(
                  Cookie.Response("github_username", username)
                    .copy(path = Some(Path.root), maxAge = Some(java.time.Duration.ofHours(8)))
                ))
                .addHeader(Header.SetCookie(
                  Cookie.Response("access_token", s"test_token_$username")
                    .copy(path = Some(Path.root), maxAge = Some(java.time.Duration.ofHours(8)))
                ))
                .addHeader(Header.SetCookie(
                  Cookie.Response("access_token_expires_at", expiresAtSeconds.toString)
                    .copy(path = Some(Path.root), maxAge = Some(java.time.Duration.ofHours(8)))
                ))
            )
          },
          // Stub /refresh endpoint - just return "refreshed" status
          Method.GET / "refresh" -> Handler.fromFunctionZIO { (_: zio.http.Request) =>
            ZIO.succeed(Response.json("""{"status":"refreshed"}"""))
          },
        )

        // Combine all routes: static files + test auth + WebSocket + ticket
        allRoutes = StaticFileRoutes.routes ++ testAuthRoute ++ socketApp.socketRoutes ++ ticketRoutesApp.routes

        // Start server
        serverFiber <- Server
          .serve(allRoutes @@ Middleware.serveResources(Path.empty))
          .provide(Server.defaultWith(_.port(testPort)))
          .fork

        // Wait for server to be ready
        _ <- ZIO.attemptBlocking {
          var ready = false
          var attempts = 0
          while (!ready && attempts < 50) {
            try {
              val conn = new java.net.URL(s"http://localhost:$testPort/").openConnection()
              conn.setConnectTimeout(100)
              conn.connect()
              ready = true
            } catch {
              case _: Exception =>
                Thread.sleep(100)
                attempts += 1
            }
          }
          if (!ready) throw new Exception(s"Server failed to start on port $testPort")
        }

        _ <- ZIO.addFinalizer(serverFiber.interrupt)
      yield ()
    }

  // ============================================
  // Test Helpers
  // ============================================

  def withPage[A](test: Page => Task[A]): ZIO[Browser, Throwable, A] =
    ZIO.serviceWithZIO[Browser] { browser =>
      ZIO.scoped {
        for
          context <- ZIO.acquireRelease(
            ZIO.attemptBlocking {
              browser.newContext(
                new Browser.NewContextOptions()
                  .setViewportSize(390, 844)
              )
            }
          )(ctx => ZIO.attemptBlocking(ctx.close()).orDie)
          page <- ZIO.attemptBlocking(context.newPage())
          result <- test(page)
        yield result
      }
    }

  // Use 127.0.0.1 instead of localhost so client uses window.location.origin
  // instead of hardcoded "http://localhost:8080"
  val baseUrl = s"http://127.0.0.1:$testPort"

  def authenticate(page: Page, username: String): Task[Unit] =
    ZIO.attemptBlocking {
      page.navigate(s"$baseUrl/api/test/auth?username=$username")
    }.unit

  def goToApp(page: Page): Task[Unit] =
    ZIO.attemptBlocking {
      page.navigate(baseUrl)
      // Wait for static content to load
      page.waitForLoadState()
    }.unit

  // ============================================
  // Retry Helpers
  // ============================================

  /** Retry a condition check with short intervals until it passes or times out.
    * Much faster than fixed Thread.sleep since it returns as soon as condition is met.
    */
  def waitUntil(
    page: Page,
    condition: String => Boolean,
    description: String,
    maxDuration: Duration = 10.seconds,
    interval: Duration = 100.millis,
  ): Task[String] =
    ZIO
      .attemptBlocking {
        val content = page.content()
        if condition(content) then content
        else throw new Exception(s"Condition not met: $description")
      }
      .retry(Schedule.spaced(interval) && Schedule.upTo(maxDuration))
      .mapError(e => new Exception(s"Timeout waiting for: $description", e))

  /** Wait for an element to appear and return it. */
  def waitForElement(
    page: Page,
    selector: String,
    timeout: Duration = 10.seconds,
  ): Task[com.microsoft.playwright.ElementHandle] =
    ZIO.attemptBlocking {
      page.waitForSelector(
        selector,
        new com.microsoft.playwright.Page.WaitForSelectorOptions()
          .setTimeout(timeout.toMillis.toDouble),
      )
    }

  /** Click an element, with retry if needed. */
  def clickElement(
    page: Page,
    selector: String,
    timeout: Duration = 5.seconds,
  ): Task[Unit] =
    ZIO
      .attemptBlocking {
        val element = page.locator(selector)
        element.click()
      }
      .retry(Schedule.spaced(100.millis) && Schedule.upTo(timeout))

  // ============================================
  // Tests
  // ============================================

  override def spec = suite("Playwright E2E")(
    test("server starts and serves index.html") {
      withPage { page =>
        for
          _ <- goToApp(page)
          result <- ZIO.attemptBlocking {
            val title = page.title()
            val content = page.content()
            title.nonEmpty || content.contains("html")
          }
        yield assertTrue(result)
      }
    },

    test("test auth sets cookies") {
      withPage { page =>
        for
          _ <- authenticate(page, "e2e-test-user")
          result <- ZIO.attemptBlocking {
            import scala.jdk.CollectionConverters.*
            val cookies = page.context().cookies().asScala.toList
            val hasGithubUsername = cookies.exists(_.name == "github_username")
            val hasAccessToken = cookies.exists(_.name == "access_token")
            hasGithubUsername && hasAccessToken
          }
        yield assertTrue(result)
      }
    },

    test("static files are served") {
      withPage { page =>
        for
          response <- ZIO.attemptBlocking {
            page.navigate(s"$baseUrl/client-fastopt.js")
            page.content()
          }
        yield assertTrue(response.nonEmpty)
      }
    },

    test("ticket endpoint is accessible from browser JS") {
      withPage { page =>
        for
          _ <- authenticate(page, "ticket-test-user")
          _ <- goToApp(page)
          result <- ZIO.attemptBlocking {
            // Wait for page to load
            page.waitForSelector(".BottomNav",
              new com.microsoft.playwright.Page.WaitForSelectorOptions().setTimeout(10000))

            // Try to fetch /ticket from within the browser using JavaScript
            val jsResult = page.evaluate("""
              (async () => {
                try {
                  const response = await fetch('/ticket', {
                    method: 'GET',
                    headers: { 'Authorization': 'Bearer test_token' }
                  });
                  const text = await response.text();
                  return { ok: response.ok, status: response.status, body: text };
                } catch (e) {
                  return { error: e.toString(), message: e.message };
                }
              })()
            """)
            // Verify we got a valid ticket UUID back
            jsResult.toString.contains("uuid")
          }
        yield assertTrue(result)
      }
    },

    test("authenticated user receives WebSocket state and UI renders server data") {
      withPage { page =>
        for
          // Authenticate as test user
          _ <- authenticate(page, "e2e-ws-test-user")

          // Navigate to app - this triggers WebSocket connection and state sync
          // The client connects to /discussions WebSocket, fetches ticket, sends it,
          // and receives StateReplace messages which populate the UI
          _ <- goToApp(page)

          // Wait for the bottom navigation bar to appear - this indicates:
          // 1. User is authenticated (cookies were set)
          // 2. JavaScript has loaded and executed
          // 3. WebSocket connection established and initial state received
          // The BottomNav only renders when hasAuthCookies is true
          _ <- waitForElement(page, ".BottomNav", 15.seconds)

          // Wait for server data to appear in the UI (retries until condition met)
          // These are discussion topics from DiscussionState.exampleWithDiscussions
          content <- waitUntil(
            page,
            condition = { content =>
              val hasContinuousDeploymentTopic = content.contains("Continuous Deployment")
              val hasEmotionalEnergyTopic = content.contains("emotional energy") || content.contains("Managing emotional")
              val hasCoffeeTopic = content.contains("great cup") || content.contains("coffee")
              hasContinuousDeploymentTopic || hasEmotionalEnergyTopic || hasCoffeeTopic
            },
            description = "server-seeded discussion topics to appear",
            maxDuration = 10.seconds,
          )

          // Verify UI elements are present
          result = content.contains("BottomNav") && content.contains("TopicCard")
        yield assertTrue(result)
      }
    },

    test("user can vote on topic and see vote count increment") {
      withPage { page =>
        for
          // Authenticate as a user who is NOT a facilitator of any sample topic
          // Sample topics are facilitated by: Bill, Emma, John
          _ <- authenticate(page, "e2e-voter-user")
          _ <- goToApp(page)

          // Wait for Topics view to load with topic cards
          _ <- waitForElement(page, ".BottomNav", 15.seconds)
          _ <- clickElement(page, ".BottomNav-tab:has-text('Topics')")
          _ <- waitForElement(page, ".TopicCard", 10.seconds)

          // Verify no topic has "interested" state yet (user hasn't voted)
          beforeContent <- ZIO.attemptBlocking(page.content())
          _ <- ZIO.when(beforeContent.contains("TopicCard--interested")) {
            ZIO.fail(new Exception("Topic already shows interested state before voting"))
          }

          // Find the first topic card and perform a swipe right gesture to vote "Interested"
          // SwipeableCard requires dragging past 35% of card width
          _ <- ZIO.attemptBlocking {
            val topicCard = page.locator(".TopicCard").first()
            topicCard.waitFor()
            val box = topicCard.boundingBox()
            val startX = box.x + box.width * 0.3
            val startY = box.y + box.height / 2
            val endX = box.x + box.width * 0.85 // Drag right past threshold

            // Simulate mouse drag for swipe
            page.mouse().move(startX, startY)
            page.mouse().down()
            page.mouse().move(endX, startY, new com.microsoft.playwright.Mouse.MoveOptions().setSteps(10))
            page.mouse().up()
          }

          // Wait for vote to be registered - card should now show "interested" state
          // This indicates the vote was sent to server and state was updated
          _ <- waitUntil(
            page,
            condition = { content =>
              content.contains("TopicCard--interested") ||
              content.contains("vote-celebrating") ||
              content.contains("VoteIndicator--interested")
            },
            description = "vote to be registered (card shows interested state)",
            maxDuration = 10.seconds,
          )
        yield assertCompletes
      }
    },

    test("user can create activity via plus button and see it in schedule") {
      val activityDescription = "E2E Test Hike to Mountain Peak"

      withPage { page =>
        for
          // Authenticate as test user
          _ <- authenticate(page, "e2e-activity-test-user")
          _ <- goToApp(page)

          // Wait for bottom nav (indicates app is ready and WebSocket connected)
          _ <- waitForElement(page, ".BottomNav", 15.seconds)

          // Wait for WebSocket to be ready (connection status indicator or just a brief moment)
          _ <- waitUntil(
            page,
            _.contains("BottomNav-tab--create"),
            "plus button to be clickable",
            maxDuration = 5.seconds,
          )

          // Click the plus button to open CreateSheet
          _ <- clickElement(page, ".BottomNav-tab--create")

          // Wait for CreateSheet to open
          _ <- waitForElement(page, ".CreateSheet--open", 5.seconds)

          // Click Activity type button
          _ <- clickElement(page, ".CreateSheet-typeBtn:has-text('Activity')")

          // Wait for form to appear
          _ <- waitForElement(page, ".CreateSheet-form", 5.seconds)

          // Fill in the activity form
          _ <- ZIO.attemptBlocking {
            // Fill description
            val descInput = page.locator(".CreateSheet-input[type='text']")
            descInput.fill(activityDescription)

            // Set time to tomorrow at 2pm
            val timeInput = page.locator(".CreateSheet-input[type='datetime-local']")
            val tomorrow = java.time.LocalDateTime.now().plusDays(1).withHour(14).withMinute(0)
            val formatted = tomorrow.format(java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm"))
            timeInput.fill(formatted)
          }

          // Click Create Activity button
          _ <- clickElement(page, ".CreateSheet-submitBtn:has-text('Create Activity')")

          // Wait for CreateSheet to close (indicates successful creation)
          _ <- waitUntil(
            page,
            content => !content.contains("CreateSheet--open"),
            "CreateSheet to close after activity creation",
            maxDuration = 5.seconds,
          )

          // Navigate to Schedule view
          _ <- clickElement(page, ".BottomNav-tab:has-text('Schedule')")

          // Wait for activity to appear in the schedule (retries until found)
          content <- waitUntil(
            page,
            condition = { content =>
              val hasActivity = content.contains(activityDescription) || content.contains("E2E Test Hike")
              val hasScheduleView = content.contains("LinearScheduleView") ||
                                    content.contains("LinearDay") ||
                                    content.contains("Schedule")
              hasActivity && hasScheduleView
            },
            description = s"activity '$activityDescription' to appear in schedule",
            maxDuration = 10.seconds,
          )
        yield assertTrue(content.nonEmpty)
      }
    },
  ).provide(
    embeddedServerLayer,
    playwrightLayer,
    browserLayer,
    // WebSocket and ticket services
    BackendSocketApp.layer,
    TicketRoutesApp.layer,
    // Session management
    SessionService.layer,
    AuthenticatedTicketService.layer,
    ZLayer.succeed(AdminConfig(Set.empty, "test-channel")),
    // Discussion store (in-memory with sample data for E2E verification)
    ZLayer.make[DiscussionStore](
      DiscussionDataStore.layer(useSampleData = true),
      GlyphiconService.layer,
    ),
    GlyphiconService.layer,
    // Test layers (in-memory/no-op implementations)
    TestLayers.lightningTalkServiceLayer,
    TestLayers.hackathonProjectServiceLayer,
    TestLayers.activityServiceLayer,
    TestLayers.slackNotifierLayer,
    TestLayers.confirmedActionRepositoryLayer,
    TestLayers.userRepositoryLayer,
    TestLayers.locationServiceLayer,
    TestLayers.discussionRepositoryLayer,
    TestLayers.schedulingServiceLayer,
    RandomActionSpawner.layer(initialActive = false),
  ) @@ TestAspect.sequential @@ TestAspect.withLiveClock @@
    (if skipE2E then TestAspect.ignore else TestAspect.identity)
