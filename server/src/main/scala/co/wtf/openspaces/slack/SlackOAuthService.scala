package co.wtf.openspaces.slack

import co.wtf.openspaces.db.UserRepository
import zio.*
import zio.http.*
import zio.json.*

import java.util.Base64

/** Response from Slack OAuth v2 token exchange */
case class SlackOAuthResponse(
  ok: Boolean,
  authed_user: Option[SlackAuthedUser],
  error: Option[String]
) derives JsonCodec

/** The authenticated user from Slack OAuth */
case class SlackAuthedUser(
  id: String,
  access_token: Option[String]
) derives JsonCodec

trait SlackOAuthService:
  /** Generate the Slack OAuth URL for user identity linking */
  def getAuthUrl(githubUsername: String): String

  /** Exchange authorization code for Slack user ID and link to GitHub user */
  def handleCallback(code: String, state: String): Task[String]

  /** Unlink Slack from a user's account */
  def unlinkSlack(githubUsername: String): Task[Unit]

class SlackOAuthServiceLive(
  config: SlackOAuthConfig,
  userRepo: UserRepository,
  client: Client
) extends SlackOAuthService:

  // Traditional OAuth 2.0 endpoints (supports user_scope for chat:write)
  private val slackOAuthUrl = "https://slack.com/oauth/v2/authorize"
  private val slackTokenUrl = URL.decode("https://slack.com/api/oauth.v2.access").getOrElse(
    throw new Exception("Invalid Slack OAuth token URL")
  )

  /** Generate the Slack OAuth URL for user identity linking.
    * Uses traditional OAuth 2.0 with user_scope for chat:write permission.
    * State parameter contains the GitHub username (simple encoding for now).
    */
  def getAuthUrl(githubUsername: String): String =
    // Simple state encoding - in production you'd want to sign/encrypt this
    val state = Base64.getUrlEncoder.encodeToString(githubUsername.getBytes("UTF-8"))
    // user_scope for permissions that act on behalf of the user
    // reactions:write allows adding reactions as the user (for auto-following threads via wave emoji)
    val userScopes = "reactions:write"
    s"$slackOAuthUrl?client_id=${config.clientId}&user_scope=${java.net.URLEncoder.encode(userScopes, "UTF-8")}&redirect_uri=${java.net.URLEncoder.encode(config.redirectUri, "UTF-8")}&state=$state"

  /** Exchange authorization code for Slack user ID and access token */
  def handleCallback(code: String, state: String): Task[String] =
    for
      // Decode state to get GitHub username
      githubUsername <- ZIO.attempt {
        new String(Base64.getUrlDecoder.decode(state), "UTF-8")
      }.mapError(e => new Exception(s"Invalid state parameter: $e"))

      // Exchange code for access token
      response <- ZIO.scoped {
        client
          .addHeader(Header.ContentType(MediaType.application.`x-www-form-urlencoded`))
          .url(slackTokenUrl)
          .post("")(
            Body.fromURLEncodedForm(Form(
              FormField.simpleField("client_id", config.clientId),
              FormField.simpleField("client_secret", config.clientSecret),
              FormField.simpleField("code", code),
              FormField.simpleField("redirect_uri", config.redirectUri),
            ))
          )
          .flatMap(_.body.asString)
      }

      // Parse response
      oauthResponse <- ZIO.fromEither(response.fromJson[SlackOAuthResponse])
        .mapError(e => new Exception(s"Failed to parse Slack OAuth response: $e - body: $response"))

      // Check for errors
      _ <- ZIO.when(!oauthResponse.ok)(
        ZIO.fail(new Exception(s"Slack OAuth error: ${oauthResponse.error.getOrElse("unknown")}"))
      )

      // Extract user info
      authedUser <- ZIO.fromOption(oauthResponse.authed_user)
        .orElseFail(new Exception("No authed_user in Slack OAuth response"))

      slackUserId = authedUser.id

      accessToken <- ZIO.fromOption(authedUser.access_token)
        .orElseFail(new Exception("No access_token for authed_user in Slack OAuth response"))

      // Link Slack user ID and access token to GitHub user
      _ <- userRepo.linkSlackAccount(githubUsername, slackUserId, accessToken)
    yield githubUsername

  def unlinkSlack(githubUsername: String): Task[Unit] =
    userRepo.unlinkSlackUserId(githubUsername)

object SlackOAuthService:
  val layer: ZLayer[SlackOAuthConfig & UserRepository & Client, Nothing, SlackOAuthService] =
    ZLayer.fromFunction((config: SlackOAuthConfig, userRepo: UserRepository, client: Client) =>
      SlackOAuthServiceLive(config, userRepo, client)
    )
