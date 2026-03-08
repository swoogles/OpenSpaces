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
          result <- ZIO.attemptBlocking {
            // Wait for bottom navigation (only renders for authenticated users after app loads)
            page.waitForSelector(".BottomNav",
              new com.microsoft.playwright.Page.WaitForSelectorOptions().setTimeout(15000))

            // Give WebSocket time to sync state (the BottomNav appears immediately but data comes async)
            // TODO This should be reworked to use ZIO retry instead of hardcoded sleep values
            Thread.sleep(3000)

            // Get the page content after WebSocket sync
            val content = page.content()

            // Verify SERVER DATA appears in the rendered UI
            // These are discussion topics from DiscussionState.exampleWithDiscussions
            // (seeded via useSampleData = true)
            // Note: Full topic text may be split across elements, so check for partial matches
            val hasContinuousDeploymentTopic = content.contains("Continuous Deployment")
            val hasEmotionalEnergyTopic = content.contains("emotional energy") || content.contains("Managing emotional")
            val hasCoffeeTopic = content.contains("great cup") || content.contains("coffee")

            // At least some of the seeded data should be present
            val hasServerData = hasContinuousDeploymentTopic || hasEmotionalEnergyTopic || hasCoffeeTopic

            // Also verify UI elements are present
            val hasBottomNav = content.contains("BottomNav")
            val hasTopicCard = content.contains("TopicCard")

            hasServerData && hasTopicCard && hasBottomNav
          }
        yield assertTrue(result)
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
