package co.wtf.openspaces

import co.wtf.openspaces.VotePosition.NotInterested
import zio.*
import zio.json.*
import zio.direct.*
import zio.http.*
import zio.http.codec.{Doc, HeaderCodec}
import zio.http.codec.HttpCodec.query
import zio.http.endpoint.Endpoint

case class ClientId(value: String):
  override def toString: String = value

case class ClientSecret(value: String):
  override def toString: String = value

case class AccessToken(
  accessToken: String,
  expiresIn: Int, // seconds
  refreshToken: String,
  refreshTokenExpiresIn: Int, // seconds
  tokenType: String = "bearer"
):
  override def toString: String = accessToken

object AccessToken:
  def parse(
    data: String
  ): AccessToken = {
    val queryParams = QueryParams.decode(data)

    // Extract individual values
    AccessToken(
      accessToken = queryParams.queryParam("access_token").getOrElse("Not found"),
      expiresIn = queryParams.queryParam("expires_in").getOrElse("Not found").toInt,
      refreshToken = queryParams.queryParam("refresh_token").getOrElse("Not found"),
      refreshTokenExpiresIn = queryParams.queryParam("refresh_token_expires_in").getOrElse("Not found").toInt,
      tokenType = queryParams.queryParam("token_type").getOrElse("Not found")
    )

  }

object Backend extends ZIOAppDefault {
  import zio.http.ChannelEvent.{ExceptionCaught, Read, UserEvent, UserEventTriggered}



  case class ApplicationState(
                               connectedUsers: Ref[List[WebSocketChannel]],
                               discussionDataStore: DiscussionDataStore,
                               glyphiconService: GlyphiconService,
                               clientId: ClientId,
                               clientSecret: ClientSecret,
                               client: Client
                             ):

    val authRoutes =
      Routes(
        // TODO Build this URL way sooner
        Method.GET / "auth" -> handler(Response.redirect(URL.decode(s"https://github.com/login/oauth/authorize?client_id=$clientId").getOrElse(???))),
        Method.GET / "callback" ->

          handler(
            (req: Request) =>

              ZIO.scoped(for {
                code <- ZIO.succeed(req.queryParam("code")).someOrFail(new Exception("No code provided in query parameters"))
                _ <- ZIO.debug("callback! Code: " + code)
                res    <- client
                  .url(
                    URL.decode("https://github.com/login/oauth")
                      .getOrElse(???)
                      .addQueryParam("client_id", clientId.value)
                      .addQueryParam("client_secret", clientSecret.value)
                      .addQueryParam("code", code)
                  )
                  .get("/access_token")
                data   <- res.body.asString.map(AccessToken.parse)
                _      <- Console.printLine(data)
              } yield Response.redirect (URL.decode("/index.html").getOrElse(throw new Exception("Bad url: /index.html" ))
              ).addCookie(Cookie.Response("access_token", data.accessToken))
              ).orDie
          ), // TODO Make sure we handle the code
      )


  object ApplicationState:
    val layer =
      ZLayer.fromZIO:
        defer:
          ApplicationState(
            Ref.make(List.empty[WebSocketChannel]).run,
            ZIO.service[DiscussionDataStore].run,
            ZIO.service[GlyphiconService].run,
            System.env("GITHUB_CLIENT_ID")
              .someOrFail(new Exception("No GITHUB_CLIENT_ID found in environment"))
              .orDie
              .map(ClientId(_))
              .run
            ,
            System.env("GITHUB_CLIENT_SECRET")
              .someOrFail(new Exception("No GITHUB_CLIENT_SECRET found in environment"))
              .orDie
              .map(ClientSecret(_))
              .run,
            ZIO.service[Client].run
          )



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