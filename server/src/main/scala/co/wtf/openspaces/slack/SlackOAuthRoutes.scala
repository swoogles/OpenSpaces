package co.wtf.openspaces.slack

import zio.*
import zio.http.*
import java.net.URLEncoder
import java.nio.charset.StandardCharsets

case class SlackOAuthRoutes(
  slackOAuthService: SlackOAuthService
):
  private def encodeQueryValue(value: String): String =
    URLEncoder.encode(value, StandardCharsets.UTF_8.toString)

  val routes: Routes[Any, Response] =
    Routes(
      // GET /slack/auth?github_username=xxx - Start Slack OAuth flow
      Method.GET / "slack" / "auth" -> handler { (req: Request) =>
        req.queryParam("github_username") match
          case None =>
            ZIO.succeed(
              Response
                .text("Missing required parameter: github_username")
                .status(Status.BadRequest)
            )
          case Some(username) =>
            val url = slackOAuthService.getAuthUrl(username)
            ZIO.succeed(Response.redirect(URL.decode(url).getOrElse(
              throw new Exception(s"Invalid Slack OAuth URL: $url")
            )))
      },

      // GET /slack/callback - Handle Slack OAuth callback
      Method.GET / "slack" / "callback" -> handler { (req: Request) =>
        val codeOpt = req.queryParam("code")
        val stateOpt = req.queryParam("state")
        val errorOpt = req.queryParam("error")

        errorOpt match
          case Some(error) =>
            // User denied access or other OAuth error
            val reason = encodeQueryValue(s"Slack OAuth error: $error")
            ZIO.succeed(Response.redirect(
              URL.decode(s"/?slack_error=$reason").getOrElse(
                throw new Exception("Bad redirect URL")
              )
            ))
          case None =>
            (codeOpt, stateOpt) match
              case (Some(code), Some(state)) =>
                slackOAuthService.handleCallback(code, state)
                  .map { _ =>
                    Response.redirect(
                      URL.decode("/?slack_linked=true").getOrElse(
                        throw new Exception("Bad redirect URL")
                      )
                    )
                  }
                  .catchAll { err =>
                    ZIO.logError(s"Slack OAuth callback failed: ${err.getMessage}") *>
                    ZIO.succeed {
                      val reason = encodeQueryValue(Option(err.getMessage).getOrElse("callback_failed"))
                      Response.redirect(
                        URL.decode(s"/?slack_error=$reason").getOrElse(
                          throw new Exception("Bad redirect URL")
                        )
                      )
                    }
                  }
              case _ =>
                ZIO.succeed(
                  Response
                    .text("Missing required parameters: code and state")
                    .status(Status.BadRequest)
                )
      },

      // POST /slack/unlink - Unlink Slack from user account
      Method.POST / "slack" / "unlink" -> handler { (req: Request) =>
        req.queryParam("github_username") match
          case None =>
            ZIO.succeed(
              Response
                .text("Missing required parameter: github_username")
                .status(Status.BadRequest)
            )
          case Some(username) =>
            slackOAuthService.unlinkSlack(username)
              .map(_ => Response.ok)
              .catchAll { err =>
                ZIO.logError(s"Slack unlink failed: ${err.getMessage}") *>
                ZIO.succeed(
                  Response
                    .text(s"Failed to unlink Slack: ${err.getMessage}")
                    .status(Status.InternalServerError)
                )
              }
      }
    )

object SlackOAuthRoutes:
  val layer: ZLayer[SlackOAuthService, Nothing, SlackOAuthRoutes] =
    ZLayer.fromFunction(SlackOAuthRoutes.apply _)
