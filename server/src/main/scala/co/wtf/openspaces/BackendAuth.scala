package co.wtf.openspaces

import zio.*
import zio.direct.*
import zio.json.*
import zio.http.*

case class ClientId(
  value: String):
  override def toString: String = value

case class ClientSecret(
  value: String):
  override def toString: String = value

case class AccessToken(
  accessToken: String,
  expiresIn: Int, // seconds
  refreshToken: String,
  refreshTokenExpiresIn: Int, // seconds
  tokenType: String = "bearer"):
  override def toString: String = accessToken

object AccessToken:
  def parse(
    data: String,
  ): AccessToken = {
    val queryParams = QueryParams.decode(data)

    // Extract individual values
    AccessToken(
      accessToken =
        queryParams.queryParam("access_token").getOrElse("Not found"),
      expiresIn = queryParams
        .queryParam("expires_in")
        .getOrElse("Not found")
        .toInt,
      refreshToken = queryParams
        .queryParam("refresh_token")
        .getOrElse("Not found"),
      refreshTokenExpiresIn = queryParams
        .queryParam("refresh_token_expires_in")
        .getOrElse("Not found")
        .toInt,
      tokenType =
        queryParams.queryParam("token_type").getOrElse("Not found"),
    )

  }

case class TicketRoutesApp(
  ticketService: AuthenticatedTicketService):
  val routes =
    Routes(
      Method.GET / "ticket" -> handler { (_: Request) =>
        defer:
          val ticket = ticketService.create.run
          Response.json(ticket.toJson)
      },
    ) @@ HandlerAspect.bearerAuthZIO { secret =>
      defer:
        ZIO.unit.run
        secret.stringValue.nonEmpty || true // TODO Real check
    }

object TicketRoutesApp:
  val layer = ZLayer.fromFunction(TicketRoutesApp.apply _)

case class ApplicationState(
  clientId: ClientId,
  clientSecret: ClientSecret,
  client: Client,
  ticketRoutesApp: TicketRoutesApp):

  val initialRoutes =
    Routes(
      // TODO Build this URL way sooner
      Method.GET / "auth" -> handler(
        Response.redirect(
          URL
            .decode(
              s"https://github.com/login/oauth/authorize?client_id=$clientId",
            )
            .getOrElse(???),
        ),
      ),
      Method.GET / "callback" ->

        handler((req: Request) =>
          ZIO
            .scoped(
              for {
                code <- ZIO
                  .succeed(req.queryParam("code"))
                  .someOrFail(
                    new Exception(
                      "No code provided in query parameters",
                    ),
                  )
                res <- client
                  .url(
                    URL
                      .decode("https://github.com/login/oauth")
                      .getOrElse(???)
                      .addQueryParam("client_id", clientId.value)
                      .addQueryParam("client_secret",
                                     clientSecret.value,
                      )
                      .addQueryParam("code", code),
                  )
                  .get("/access_token")
                data <- res.body.asString.map(AccessToken.parse)
              } yield Response
                .redirect(
                  URL
                    .decode("/index.html")
                    .getOrElse(
                      throw new Exception("Bad url: /index.html"),
                    ),
                )
                .addCookie(
                  Cookie.Response("access_token", data.accessToken),
                ),
            )
            .orDie,
        ), // TODO Make sure we handle the code
    )

  val authRoutes = initialRoutes ++ ticketRoutesApp.routes

object ApplicationState:
  val layer =
    ZLayer.fromZIO:
      defer:
        ApplicationState(
          System
            .env("GITHUB_CLIENT_ID")
            .someOrFail(
              new Exception(
                "No GITHUB_CLIENT_ID found in environment",
              ),
            )
            .orDie
            .map(ClientId(_))
            .run,
          System
            .env("GITHUB_CLIENT_SECRET")
            .someOrFail(
              new Exception(
                "No GITHUB_CLIENT_SECRET found in environment",
              ),
            )
            .orDie
            .map(ClientSecret(_))
            .run,
          ZIO.service[Client].run,
          ZIO.service[TicketRoutesApp].run,
        )
