package co.wtf.openspaces

import co.wtf.openspaces.db.*
import co.wtf.openspaces.slack.*
import zio.*
import zio.direct.*
import zio.http.*
import zio.http.endpoint.openapi._
import  zio.http.endpoint.openapi._

case class MyConfig(ldap: String, port: Int, dburl: String)

object Backend extends ZIOAppDefault {

  override def run =
    val port = sys.env
      .getOrElse(
        "PORT",
        throw new IllegalStateException("No value found for $PORT"),
      )
      .toInt

    val openAPI       = OpenAPIGen.fromEndpoints(title = "Library API", version = "1.0", 
    RandomActionApi.endpoints
    )
    
    val swaggerRoutes = SwaggerUI.routes("docs", openAPI)
    
    defer:
      val statefulRoutes =
        ZIO.serviceWith[ApplicationState](_.authRoutes).run
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
      DiscussionService.layer,
      SchedulingService.layer,
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
      EventRepository.layer,
      DiscussionRepository.layer,
      PersistentDiscussionStore.layer,
      // Slack integration
      SlackConfigEnv.layer,
      SlackNotifier.layer,
    )
}
