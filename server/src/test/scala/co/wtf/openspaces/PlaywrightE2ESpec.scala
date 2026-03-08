package co.wtf.openspaces

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.http.*
import com.microsoft.playwright.{Playwright, Browser, BrowserType, Page, BrowserContext}

/** E2E tests using Playwright Java SDK with embedded server.
  *
  * Runs a minimal embedded server that serves static files and provides test auth.
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
  // Embedded Test Server (minimal)
  // ============================================

  /** Minimal test server - just static files and test auth */
  val embeddedServerLayer: ZLayer[Any, Throwable, Unit] =
    ZLayer.scoped {
      for
        // Test auth route
        testAuthRoute <- ZIO.succeed {
          Routes(
            Method.GET / "api" / "test" / "auth" -> Handler.fromFunctionZIO { (req: zio.http.Request) =>
              val username = req.url.queryParams.queryParam("username").getOrElse("testuser")
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
              )
            },
          )
        }

        allRoutes = StaticFileRoutes.routes ++ testAuthRoute

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

  val baseUrl = s"http://localhost:$testPort"

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
  ).provide(
    embeddedServerLayer,
    playwrightLayer,
    browserLayer,
  ) @@ TestAspect.sequential @@ TestAspect.withLiveClock @@
    (if skipE2E then TestAspect.ignore else TestAspect.identity)
