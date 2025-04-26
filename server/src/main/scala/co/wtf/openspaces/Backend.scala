package co.wtf.openspaces

import zio.*
import zio.direct.*
import zio.http.*

object Backend extends ZIOAppDefault {

  override def run =
    val port = sys.env
      .getOrElse(
        "PORT",
        throw new IllegalStateException("No value found for $PORT"),
      )
      .toInt
    defer:
      val statefulRoutes =
        ZIO.serviceWith[ApplicationState](_.authRoutes).run
      val socketRoutes =
        ZIO.serviceWith[BackendSocketApp](_.socketRoutes).run
      val allRoutes = statefulRoutes ++ socketRoutes

      Server
        .serve(allRoutes @@ Middleware.serveResources(Path.empty))
        .as("Just working around zio-direct limitation")
        .run
    .provide(
      Server.defaultWith(_.port(port)),
      ApplicationState.layer,
      BackendSocketApp.layer,
      DiscussionService.layer,
      DiscussionDataStore.layerWithSampleData,
      GlyphiconService.layer,
      Client.default,
      AuthenticatedTicketService.layer,
      TicketRoutesApp.layer,
    )
}
