package co.wtf.openspaces

import co.wtf.openspaces.VotePosition.NotInterested
import zio.*
import zio.json.*
import zio.direct.*
import zio.http.*
import zio.http.codec.{Doc, HeaderCodec}
import zio.http.codec.HttpCodec.query
import zio.http.endpoint.Endpoint


object Backend extends ZIOAppDefault {
  import zio.http.ChannelEvent.{ExceptionCaught, Read, UserEvent, UserEventTriggered}




  override def run =
    val port = sys.env.getOrElse("PORT", throw new IllegalStateException("No value found for $PORT")).toInt
    defer:
      val statefulRoutes = ZIO.serviceWith[ApplicationState](_.authRoutes).run
      val socketRoutes = ZIO.serviceWith[BackendSocketApp](_.socketRoutes).run
      val allRoutes = statefulRoutes ++ socketRoutes

      Server.serve(allRoutes @@ Middleware.serveResources(Path.empty))
        .as("Just working around zio-direct limitation").run
    .provide(
      Server.defaultWith(_.port(port)),
      ApplicationState.layer,
      BackendSocketApp.layer,
      DiscussionDataStore.layer,
      GlyphiconService.live,
      Client.default,
    )
}