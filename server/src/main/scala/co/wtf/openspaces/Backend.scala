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

    val socketApp: WebSocketApp[Any] =
      Handler.webSocket { channel =>
        channel.receiveAll {
          case Read(WebSocketFrame.Text(text)) =>
            defer:
              val discussionAction = ZIO.fromEither(text.fromJson[DiscussionAction])
                .mapError(deserError => new Exception(s"Failed to deserialize: $deserError"))
                .run

              val actionResult = discussionDataStore.applyAction(discussionAction).run
              defer:
                val channels = connectedUsers.get.run
                println(s"Action result: \n $actionResult\nWill send to ${channels.size} connected users.")
                ZIO.foreachParDiscard(channels)( channel =>
                  val fullJson = actionResult.toJsonPretty
                  ZIO.debug(s"Sending discussion: $fullJson to $channel") *>
                    channel.send(Read(WebSocketFrame.text(fullJson))).ignore
                ).run
              .run

//            .catchAll(ex => ZIO.debug("Failed to handle action: " + ex)) // TODO Does this screw up the socket connection if we let errors escape?

          case UserEventTriggered(UserEvent.HandshakeComplete) =>
            defer:
              connectedUsers.update(_ :+ channel).run
              val discussions = discussionDataStore.snapshot.run
              ZIO.foreachDiscard(discussions.data)((topic, discussion) =>
                val content: DiscussionActionConfirmed = DiscussionActionConfirmed.AddResult(discussion)
                channel.send(Read(WebSocketFrame.text(content.toJson)))
              ).run

              ZIO.when(false):
                defer:
                  val action = discussionDataStore.randomDiscussionAction.run
                  channel.send(Read(WebSocketFrame.text(action.toJson))).run
                .repeat(Schedule.spaced(1.seconds) && Schedule.forever)
              .forkDaemon.run

          case Read(WebSocketFrame.Close(status, reason)) =>
            Console.printLine("Closing channel with status: " + status + " and reason: " + reason)

          case ExceptionCaught(cause) =>
            Console.printLine(s"Channel error!: ${cause.getMessage}")

          case other =>
            ZIO.debug("Other channel event: " + other)
        }
      }


    // TODO Consider "ticketing" system described here for WS auth
    // https://devcenter.heroku.com/articles/websocket-security

    val socketRoutes =
      Routes(
        Method.GET / "discussions"          -> handler(socketApp.toResponse),
      )
      /*
      // This might actually be the backend bit that I want, but I don't know how to pass it via the front end.
        @@ HandlerAspect.bearerAuthZIO { secret =>
        defer:
          ZIO.debug("TODO: Actual logic on this token: " + secret.stringValue).run
          secret.stringValue.nonEmpty || true // TODO Real check
      }

       */

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

    val allRoutes =
      socketRoutes ++
        authRoutes


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
      val statefulRoutes = ZIO.serviceWith[ApplicationState](_.allRoutes).run

      Server.serve(statefulRoutes @@ Middleware.serveResources(Path.empty))
        .as("Just working around zio-direct limitation").run
    .provide(
      Server.defaultWith(_.port(port)),
      ApplicationState.layer,
      DiscussionDataStore.layer,
      GlyphiconService.live,
      Client.default,
    )
}