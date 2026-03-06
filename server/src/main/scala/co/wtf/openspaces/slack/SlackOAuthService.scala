package co.wtf.openspaces.slack

import co.wtf.openspaces.db.UserRepository
import zio.*
import zio.http.*
import zio.json.*

import java.util.Base64

/** Response from Slack OpenID Connect token exchange */
case class SlackOIDCTokenResponse(
  ok: Boolean,
  access_token: Option[String],
  token_type: Option[String],
  id_token: Option[String],
  error: Option[String]
) derives JsonCodec

/** Claims from the Slack OIDC id_token JWT (subset we care about) */
case class SlackIdTokenClaims(
  sub: String,                    // Slack user ID (e.g., "U1234567890")
  `https://slack.com/team_id`: Option[String],
  email: Option[String],
  name: Option[String],
  picture: Option[String]
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

  // OIDC endpoints for Sign in with Slack
  private val slackOIDCAuthUrl = "https://slack.com/openid/connect/authorize"
  private val slackOIDCTokenUrl = URL.decode("https://slack.com/api/openid.connect.token").getOrElse(
    throw new Exception("Invalid Slack OIDC token URL")
  )

  /** Generate the Slack OIDC authorization URL.
    * Uses OpenID Connect scopes for modern Sign in with Slack.
    * State parameter contains the GitHub username (simple encoding for now).
    */
  def getAuthUrl(githubUsername: String): String =
    // Simple state encoding - in production you'd want to sign/encrypt this
    val state = Base64.getUrlEncoder.encodeToString(githubUsername.getBytes("UTF-8"))
    val scopes = "openid email profile"
    s"$slackOIDCAuthUrl?client_id=${config.clientId}&scope=${java.net.URLEncoder.encode(scopes, "UTF-8")}&redirect_uri=${java.net.URLEncoder.encode(config.redirectUri, "UTF-8")}&state=$state&response_type=code"

  /** Exchange authorization code for Slack user ID via OIDC */
  def handleCallback(code: String, state: String): Task[String] =
    for
      // Decode state to get GitHub username
      githubUsername <- ZIO.attempt {
        new String(Base64.getUrlDecoder.decode(state), "UTF-8")
      }.mapError(e => new Exception(s"Invalid state parameter: $e"))

      // Exchange code for OIDC tokens
      response <- ZIO.scoped {
        client
          .addHeader(Header.ContentType(MediaType.application.`x-www-form-urlencoded`))
          .url(slackOIDCTokenUrl)
          .post("")(
            Body.fromURLEncodedForm(Form(
              FormField.simpleField("client_id", config.clientId),
              FormField.simpleField("client_secret", config.clientSecret),
              FormField.simpleField("code", code),
              FormField.simpleField("redirect_uri", config.redirectUri),
              FormField.simpleField("grant_type", "authorization_code"),
            ))
          )
          .flatMap(_.body.asString)
      }

      // Parse response
      tokenResponse <- ZIO.fromEither(response.fromJson[SlackOIDCTokenResponse])
        .mapError(e => new Exception(s"Failed to parse Slack OIDC response: $e - body: $response"))

      // Check for errors
      _ <- ZIO.when(!tokenResponse.ok)(
        ZIO.fail(new Exception(s"Slack OIDC error: ${tokenResponse.error.getOrElse("unknown")}"))
      )

      // Extract and decode the id_token JWT to get the Slack user ID
      idToken <- ZIO.fromOption(tokenResponse.id_token)
        .orElseFail(new Exception("No id_token in Slack OIDC response"))

      claims <- decodeIdToken(idToken)

      slackUserId = claims.sub

      // Link Slack user ID to GitHub user
      _ <- userRepo.linkSlackUserId(githubUsername, slackUserId)
    yield githubUsername

  /** Decode the JWT id_token payload (we trust Slack's signature, just extract claims) */
  private def decodeIdToken(idToken: String): Task[SlackIdTokenClaims] =
    ZIO.attempt {
      // JWT format: header.payload.signature
      val parts = idToken.split("\\.")
      if parts.length != 3 then
        throw new Exception("Invalid JWT format")

      // Decode the payload (middle part) - use URL-safe Base64 decoder
      val payloadJson = new String(Base64.getUrlDecoder.decode(parts(1)), "UTF-8")
      payloadJson.fromJson[SlackIdTokenClaims] match
        case Right(claims) => claims
        case Left(err) => throw new Exception(s"Failed to parse id_token claims: $err")
    }

  def unlinkSlack(githubUsername: String): Task[Unit] =
    userRepo.unlinkSlackUserId(githubUsername)

object SlackOAuthService:
  val layer: ZLayer[SlackOAuthConfig & UserRepository & Client, Nothing, SlackOAuthService] =
    ZLayer.fromFunction((config: SlackOAuthConfig, userRepo: UserRepository, client: Client) =>
      SlackOAuthServiceLive(config, userRepo, client)
    )
