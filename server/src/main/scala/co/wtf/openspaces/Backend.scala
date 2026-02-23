package co.wtf.openspaces

import co.wtf.openspaces.auth.AuthenticatedTicketService
import co.wtf.openspaces.db._
import co.wtf.openspaces.discussions.SchedulingService
import co.wtf.openspaces.github.GitHubProfileService
import co.wtf.openspaces.hackathon.HackathonProjectService
import co.wtf.openspaces.lightning_talks.LightningTalkService
import co.wtf.openspaces.slack._
import zio.direct._
import zio.http._
import zio.http.endpoint.openapi._
import java.time.format.DateTimeFormatter

import zio.json.EncoderOps
import zio.logging.{ConsoleLoggerConfig, LogAnnotation, LogFormat, consoleJsonLogger}
import zio.logging.LogFormat._
import zio._

case class MyConfig(ldap: String, port: Int, dburl: String)

object Backend extends ZIOAppDefault {

  private val userLogAnnotation = LogAnnotation[UserRow]("user", (_, u) => u, _.toJson)
  
  
  val myLogFormat =
    label("timestamp", LogFormat.timestamp(DateTimeFormatter.ofPattern("MM-dd HH:mm:ss"))) |-|
      label("level", LogFormat.level) |-|
      label("message", quoted(line)) |-|
      LogFormat.annotation(userLogAnnotation)
  
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
      val allRoutes = statefulRoutes ++ socketRoutes ++ randomActionRoutes ++ swaggerRoutes

      // Start the random action spawner (runs in background, controlled via admin API)
      ZIO.serviceWithZIO[RandomActionSpawner](_.startSpawningRandomActions).run

      Server
        .serve(allRoutes @@ Middleware.serveResources(Path.empty))
        .as("Just working around zio-direct limitation")
        .run
    .provide(
      Server.defaultWith(_.port(port)),
      ApplicationState.layer,
      BackendSocketApp.layer,
      SessionService.layer,
      SchedulingService.layer,
      LightningTalkService.layer,
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
      GitHubProfileService.layer,
      EventRepository.layer,
      DiscussionRepository.layer,
      TopicVoteRepository.layer,
      LightningTalkRepository.layer,
      HackathonProjectRepository.layer,
      HackathonProjectMemberRepository.layer,
      ConfirmedActionRepository.layer,
      PersistentDiscussionStore.layer,
      // Hackathon projects (Wednesday)
      HackathonProjectService.layer,
      // Slack integration
      SlackConfigEnv.layer,
      SlackNotifier.layer,
    )
}
