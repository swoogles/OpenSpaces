package co.wtf.openspaces

import co.wtf.openspaces.auth.{AdminConfig, AuthenticatedTicketService}
import co.wtf.openspaces.db.UserRepository
import co.wtf.openspaces.slack.SlackNotifier
import zio.*
import zio.direct.*
import zio.json.*
import zio.http.*
import java.time.Instant

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
        .getOrElse("28800") // Default 8 hours if not provided
        .toInt,
      refreshToken = queryParams
        .queryParam("refresh_token")
        .getOrElse("Not found"),
      refreshTokenExpiresIn = queryParams
        .queryParam("refresh_token_expires_in")
        .getOrElse("15811200") // Default ~6 months if not provided
        .toInt,
      tokenType =
        queryParams.queryParam("token_type").getOrElse("Not found"),
    )
  }

  /** Parse JSON response from GitHub token refresh endpoint */
  def parseJson(json: String): Either[String, AccessToken] =
    json.fromJson[AccessTokenJson].map(_.toAccessToken)

/** JSON codec for GitHub OAuth token response */
case class AccessTokenJson(
  access_token: String,
  expires_in: Option[Int],
  refresh_token: String,
  refresh_token_expires_in: Option[Int],
  token_type: Option[String]
) derives JsonCodec:
  def toAccessToken: AccessToken = AccessToken(
    accessToken = access_token,
    expiresIn = expires_in.getOrElse(28800),
    refreshToken = refresh_token,
    refreshTokenExpiresIn = refresh_token_expires_in.getOrElse(15811200),
    tokenType = token_type.getOrElse("bearer")
  )

/** Helper to create auth cookies with proper expiration */
object AuthCookies:
  /** Create a response cookie with proper settings for auth tokens */
  def create(
    name: String,
    value: String,
    maxAgeSeconds: Int,
    httpOnly: Boolean = false
  ): Cookie.Response =
    Cookie.Response(
      name = name,
      content = value,
      maxAge = Some(java.time.Duration.ofSeconds(maxAgeSeconds.toLong)),
      path = Some(Path.root),
      isHttpOnly = httpOnly,
      // In production, set isSecure = true for HTTPS
    )

  /** Create all auth cookies from token data */
  def fromToken(
    token: AccessToken,
    username: String
  ): Seq[Cookie.Response] = Seq(
    create("access_token", token.accessToken, token.refreshTokenExpiresIn),
    create("refresh_token", token.refreshToken, token.refreshTokenExpiresIn, httpOnly = true),
    create("github_username", username, token.refreshTokenExpiresIn),
    // Store expiry timestamp so client/server can check if refresh is needed
    create("access_token_expires_at",
      (Instant.now().getEpochSecond + token.expiresIn).toString,
      token.refreshTokenExpiresIn)
  )

/** Represents the GitHub user profile response.
  * - login: GitHub username (always present)
  * - name: Display name (optional, user can leave blank)
  */
case class GitHubUser(
  login: String,
  name: Option[String] = None)
    derives JsonCodec

case class TicketRoutesApp(
  ticketService: AuthenticatedTicketService):
  val routes =
    Routes(
      RandomActionApi.ticketGet.implement { _ =>
        ticketService.create
      },
    )

object TicketRoutesApp:
  val layer = ZLayer.fromFunction(TicketRoutesApp.apply _)

case class ApplicationState(
  clientId: ClientId,
  clientSecret: ClientSecret,
  client: Client,
  ticketRoutesApp: TicketRoutesApp,
  userRepository: UserRepository,
  slackNotifier: slack.SlackNotifier,
  adminConfig: auth.AdminConfig):

  val authRoutes =
    Routes(
      // TODO Build this URL way sooner
      Method.GET / "auth" -> handler {
        ZIO
          .fromEither(
            URL.decode(
              s"https://github.com/login/oauth/authorize?client_id=$clientId",
            ),
          )
          .mapBoth(
            err => new Exception(s"Failed to build GitHub auth URL: $err"),
            url => Response.redirect(url),
          )
          .tapErrorCause(cause => ZIO.logErrorCause("Auth route failed", cause))
          .catchAllCause { _ =>
            ZIO.succeed(
              Response.redirect(
                URL
                  .decode("/?auth_error=auth_route_failed")
                  .getOrElse(throw new Exception("Bad url: /?auth_error=auth_route_failed"))
              )
            )
          }
      },
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
                // Fetch GitHub username using the access token
                userRes <- client
                  .addHeader(Header.Authorization.Bearer(data.accessToken))
                  .addHeader(Header.Accept(MediaType.application.json))
                  .addHeader(Header.Custom("User-Agent", "openspaces-app"))
                  .url(
                    URL
                      .decode("https://api.github.com")
                      .getOrElse(???),
                  )
                  .get("/user")
                userBody <- userRes.body.asString
                githubUser <- ZIO
                  .fromEither(userBody.fromJson[GitHubUser])
                  .mapError(e => new Exception(s"Failed to parse GitHub user: $e"))
                // Upsert user to database (creates if new, updates display name if changed)
                upsertResult <- userRepository.upsert(
                  githubUser.login,
                  githubUser.name.map(_.trim).filter(_.nonEmpty).orElse(Some(githubUser.login)),
                )
                // If new user and not approved (and not an admin), notify Slack
                _ <- ZIO.when(upsertResult.isNewUser && !upsertResult.user.approved && !adminConfig.isAdmin(githubUser.login))(
                  slackNotifier.notifyAccessRequest(
                    githubUser.login,
                    githubUser.name.map(_.trim).filter(_.nonEmpty)
                  )
                )
              } yield {
                val cookies = AuthCookies.fromToken(data, githubUser.login)
                cookies.foldLeft(
                  Response.redirect(
                    URL.decode("/").getOrElse(throw new Exception("Bad url: /"))
                  )
                )((response, cookie) => response.addCookie(cookie))
                  .addCookie(
                    Cookie.Response("access_token", data.accessToken),
                  )
                  .addCookie(
                    Cookie.Response("github_username", githubUser.login),
                  )
              }
            )
            .tapErrorCause(cause => ZIO.logErrorCause("OAuth callback failed", cause))
            .catchAllCause { _ =>
              ZIO.succeed(
                Response.redirect(
                  URL
                    .decode("/?auth_error=callback_failed")
                    .getOrElse(throw new Exception("Bad url: /?auth_error=callback_failed"))
                )
              )
            },
        ),
      RandomActionApi.refreshGet.implement { cookieHeaderOpt =>
        def cookieValue(name: String): Option[String] =
          cookieHeaderOpt
            .map(_.value.toChunk)
            .getOrElse(Chunk.empty)
            .find(_.name == name)
            .map(_.content)

        def clearCookieHeader(name: String): Header.SetCookie =
          Header.SetCookie(
            Cookie.Response(
              name,
              "",
              maxAge = Some(java.time.Duration.ZERO),
              path = Some(Path.root),
            ),
          )

        val clearedAccessToken = clearCookieHeader("access_token")
        val clearedRefreshToken = clearCookieHeader("refresh_token")
        val clearedGithubUsername = clearCookieHeader("github_username")
        val clearedExpiresAt = clearCookieHeader("access_token_expires_at")

        ZIO
          .scoped(
            for {
              _ <- ZIO.debug("Zio Endpoint refresh")
              refreshToken <- ZIO
                .fromOption(cookieValue("refresh_token"))
                .orElseFail(new Exception("No refresh token found in cookies"))
              username <- ZIO
                .fromOption(cookieValue("github_username"))
                .orElseFail(new Exception("No github_username cookie found"))
              // Request new tokens using refresh_token grant
              res <- client
                .addHeader(Header.Accept(MediaType.application.json))
                .url(
                  URL
                    .decode("https://github.com/login/oauth/access_token")
                    .getOrElse(???),
                )
                .post("/")(
                  Body.fromURLEncodedForm(
                    Form(
                      FormField.simpleField("client_id", clientId.value),
                      FormField.simpleField("client_secret", clientSecret.value),
                      FormField.simpleField("grant_type", "refresh_token"),
                      FormField.simpleField("refresh_token", refreshToken),
                    ),
                  ),
                )
              body <- res.body.asString
              data <- ZIO
                .fromEither(AccessToken.parseJson(body))
                .mapError(e => new Exception(s"Failed to parse token response: $e - body: $body"))
              cookies = AuthCookies.fromToken(data, username)
              accessTokenCookie <- ZIO
                .fromOption(cookies.find(_.name == "access_token"))
                .orElseFail(new Exception("Missing refreshed access token cookie"))
              refreshTokenCookie <- ZIO
                .fromOption(cookies.find(_.name == "refresh_token"))
                .orElseFail(new Exception("Missing refreshed refresh token cookie"))
              usernameCookie <- ZIO
                .fromOption(cookies.find(_.name == "github_username"))
                .orElseFail(new Exception("Missing refreshed github_username cookie"))
              expiresAtCookie <- ZIO
                .fromOption(cookies.find(_.name == "access_token_expires_at"))
                .orElseFail(new Exception("Missing refreshed expiry cookie"))
            } yield (
              RefreshStatus("refreshed"),
              Header.SetCookie(accessTokenCookie),
              Header.SetCookie(refreshTokenCookie),
              Header.SetCookie(usernameCookie),
              Header.SetCookie(expiresAtCookie),
            ),
          )
          .catchAll { _ =>
            ZIO.succeed(
              (
                RefreshStatus("reauth_required"),
                clearedAccessToken,
                clearedRefreshToken,
                clearedGithubUsername,
                clearedExpiresAt,
              ),
            )
          }
      },
    )

  val routes = StaticFileRoutes.routes ++ authRoutes ++ ticketRoutesApp.routes

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
          ZIO.service[UserRepository].run,
          ZIO.service[SlackNotifier].run,
          ZIO.service[AdminConfig].run,
        )
