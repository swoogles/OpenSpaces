package co.wtf.openspaces

import co.wtf.openspaces.auth.{AdminConfig, AuthenticatedTicketService, AuthRoutes}
import co.wtf.openspaces.db._
import co.wtf.openspaces.discussions.SchedulingService
import co.wtf.openspaces.github.GitHubProfileService
import co.wtf.openspaces.hackathon.HackathonProjectService
import co.wtf.openspaces.lightning_talks.LightningTalkService
import co.wtf.openspaces.activities.ActivityService
import co.wtf.openspaces.location.LocationService
import co.wtf.openspaces.slack._
import zio.direct._
import zio.http._
import zio.http.endpoint.openapi._
import java.time.format.DateTimeFormatter

import zio.json.EncoderOps
import zio.logging.{ConsoleLoggerConfig, LogAnnotation as BackendLogAnnotation, LogFormat, consoleJsonLogger}
import zio.logging.LogFormat._
import zio._

case class MyConfig(ldap: String, port: Int, dburl: String)

object Backend extends ZIOAppDefault {

  private val userLogAnnotation = BackendLogAnnotation[UserRow]("user", (_, u) => u, _.toJson)
  
  
  val myLogFormat =
    label("timestamp", LogFormat.timestamp(DateTimeFormatter.ofPattern("MM-dd HH:mm:ss"))) |-|
      label("level", LogFormat.level) |-|
      label("message", quoted(line)) |-|
      LogFormat.annotation(userLogAnnotation) |-|
      LogFormat.annotations |-|
      label("cause", LogFormat.cause)
  
  private val logConfig = ConsoleLoggerConfig.default.copy(
    format =  myLogFormat
    // (LogFormat.timestamp(DateTimeFormatter.ISO_LOCAL_TIME) +
    // LogFormat.space +
    // LogFormat.level +
    // LogFormat.space +
    //       LogFormat.annotation(LogAnnotation.TraceId) + LogFormat.annotation(userLogAnnotation))
  )
  
  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] =
    Runtime.removeDefaultLoggers >>> consoleJsonLogger(logConfig)

  override def run =
    val port = sys.env
      .getOrElse(
        "PORT",
        throw new IllegalStateException("No value found for $PORT"),
      )
      .toInt

    val openAPI       = OpenAPIGen.fromEndpoints(title = "server", version = "1.0", 
    RandomActionApi.endpoints
    )
    
    val swaggerRoutes = SwaggerUI.routes("docs", openAPI)
    
    defer:
      val statefulRoutes =
        ZIO.serviceWith[ApplicationState](_.routes).run
      val socketRoutes =
        ZIO.serviceWith[BackendSocketApp](_.socketRoutes).run
      val randomActionRoutes =
        ZIO.serviceWith[RandomActionSpawner](_.routes).run
      val authRoutes =
        ZIO.serviceWith[AuthRoutes](_.routes).run
      // Slack OAuth routes are optional - only available if SLACK_CLIENT_ID is configured
      val slackOAuthRoutes =
        ZIO.serviceWith[Option[SlackOAuthRoutes]](_.map(_.routes).getOrElse(Routes.empty)).run
      val allRoutes = statefulRoutes ++ socketRoutes ++ randomActionRoutes ++ authRoutes ++ slackOAuthRoutes ++ swaggerRoutes

      // Start the random action spawner (runs in background, controlled via admin API)
      ZIO.serviceWithZIO[RandomActionSpawner](_.startSpawningRandomActions).run

      // Run the real auto-scheduler every 30 minutes, anchored to 2026-03-03 09:00 MST.
      ZIO.serviceWithZIO[SchedulingService](_.startAutomaticScheduling).run
      
      // Start the Slack reply count refresh (runs in background, gracefully handles missing permissions)
      // 5-minute interval to stay well within Slack rate limits
      ZIO.serviceWithZIO[SessionService](_.startSlackReplyCountRefresh(interval = 5.minutes)).run

      // Keep websocket connections active so Heroku does not drop them for idleness.
      ZIO.serviceWithZIO[SessionService](_.startWebSocketKeepAlive(interval = 25.seconds)).run

      // Start location sharing expiration check (every 10 minutes)
      ZIO.serviceWithZIO[SessionService](_.startLocationExpirationCheck(interval = 10.minutes)).run

      val requestLogAnnotations =
        Middleware.logAnnotate((req: Request) =>
          Set(
            zio.LogAnnotation("method", req.method.toString),
            zio.LogAnnotation("path", req.path.encode),
          )
        )

      Server
        .serve(allRoutes @@ Middleware.serveResources(Path.empty) @@ requestLogAnnotations)
        .as("Just working around zio-direct limitation")
        .run
    .provide(
      Server.defaultWith(_.port(port)),
      ApplicationState.layer,
      BackendSocketApp.layer,
      SessionService.layer,
      SchedulingService.layer,
      LightningTalkService.layer,
      ActivityService.layer,
      LocationService.layer,
      RandomActionSpawner.layer(initialActive = false),
      GlyphiconService.layer,
      Client.default,
      AuthenticatedTicketService.layer,
      TicketRoutesApp.layer,
      // Database layers
      DbConfig.layer,
      DataSourceLive.layer,
      FlywayMigration.layer,
      UserRepository.layer,
      RoomRepository.layer,
      TimeSlotRepository.layer,
      GitHubProfileService.layer,
      EventRepository.layer,
      DiscussionRepository.layer,
      TopicVoteRepository.layer,
      LightningTalkRepository.layer,
      HackathonProjectRepository.layer,
      HackathonProjectMemberRepository.layer,
      ActivityRepository.layer,
      ActivityInterestRepository.layer,
      ConfirmedActionRepository.layer,
      PersistentDiscussionStore.layer,
      // Hackathon projects (Wednesday)
      HackathonProjectService.layer,
      // Slack integration
      SlackConfigEnv.layer,
      SlackNotifier.layer,
      SlackRosterMessageRepository.layer,
      // Slack OAuth (optional - requires SLACK_CLIENT_ID and SLACK_CLIENT_SECRET)
      SlackOAuthConfig.layer,
      ZLayer.fromZIO(
        for
          configOpt <- ZIO.service[Option[SlackOAuthConfig]]
          serviceOpt <- configOpt match
            case Some(config) =>
              for
                userRepo <- ZIO.service[UserRepository]
                client <- ZIO.service[Client]
              yield Some(SlackOAuthServiceLive(config, userRepo, client): SlackOAuthService)
            case None =>
              ZIO.logInfo("Slack OAuth not configured (missing SLACK_CLIENT_ID or SLACK_CLIENT_SECRET)") *>
              ZIO.succeed(None)
        yield serviceOpt
      ),
      ZLayer.fromZIO(
        ZIO.serviceWith[Option[SlackOAuthService]](_.map(SlackOAuthRoutes.apply))
      ),
      // Authorization
      AdminConfig.layer,
      AuthRoutes.layer,
    )
}
