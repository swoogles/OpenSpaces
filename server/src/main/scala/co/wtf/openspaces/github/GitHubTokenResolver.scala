package co.wtf.openspaces.github

import zio.*
import zio.http.*
import zio.json.*

private case class GitHubAuthenticatedUser(login: String) derives JsonCodec

/** Resolves a GitHub OAuth access token to the GitHub login that owns it.
  *
  * Used when issuing WebSocket tickets, so a ticket is bound to the user who
  * actually holds the token rather than to whatever username the client claims.
  */
trait GitHubTokenResolver:
  def loginFor(accessToken: String): Task[String]

object GitHubTokenResolver:
  /** Strip an optional "Bearer " prefix; the browser client sends the raw token. */
  def normalizeToken(header: String): String =
    header.trim.stripPrefix("Bearer ").trim

  val layer: ZLayer[Client, Nothing, GitHubTokenResolver] =
    ZLayer.fromZIO:
      for
        client   <- ZIO.service[Client]
        cache    <- Ref.make(Map.empty[String, String])
        testMode <- zio.System.env("TEST_MODE").orDie.map(_.contains("true"))
      yield GitHubTokenResolverLive(client, cache, testMode)

  /** For tests: tokens of the form `test_token_<login>` resolve to `<login>`. */
  val testLayer: ULayer[GitHubTokenResolver] =
    ZLayer.succeed(new GitHubTokenResolver:
      def loginFor(accessToken: String): Task[String] =
        TestTokens.loginFor(accessToken)
    )

/** Mirrors the tokens minted by the TEST_MODE /api/test/auth route. */
private object TestTokens:
  val Prefix = "test_token_"

  def loginFor(accessToken: String): Task[String] =
    ZIO
      .succeed(accessToken.stripPrefix(Prefix))
      .filterOrFail(login => accessToken.startsWith(Prefix) && login.nonEmpty)(
        new Exception("Not a test token"),
      )

class GitHubTokenResolverLive(
  client: Client,
  cache: Ref[Map[String, String]],
  testMode: Boolean,
) extends GitHubTokenResolver:

  def loginFor(accessToken: String): Task[String] =
    if accessToken.isEmpty then ZIO.fail(new Exception("Missing access token"))
    else if testMode && accessToken.startsWith(TestTokens.Prefix) then TestTokens.loginFor(accessToken)
    else
      cache.get.map(_.get(accessToken)).someOrElseZIO(
        fetchLogin(accessToken).tap(login => cache.update(_ + (accessToken -> login))),
      )

  private def fetchLogin(accessToken: String): Task[String] =
    ZIO.scoped:
      for
        response <- client
          .addHeader(Header.Accept(MediaType.application.json))
          .addHeader(Header.Custom("User-Agent", "openspaces-app"))
          .addHeader(Header.Authorization.Bearer(accessToken))
          .url(URL.decode("https://api.github.com").getOrElse(throw new Exception("Bad GitHub API URL")))
          .get("/user")
        _ <- ZIO.unless(response.status.isSuccess)(
          ZIO.fail(new Exception(s"GitHub rejected access token (status ${response.status.code})")),
        )
        body <- response.body.asString
        user <- ZIO
          .fromEither(body.fromJson[GitHubAuthenticatedUser])
          .mapError(e => new Exception(s"Failed to parse GitHub /user response: $e"))
      yield user.login
