package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import scala.scalajs.js
import scala.concurrent.ExecutionContext.Implicits.global

/** Button that copies @mentions to clipboard and opens the Slack thread.
  *
  * Note: Currently a placeholder - full implementation requires exposing Slack user IDs
  * to the frontend. For now, this just opens the Slack thread.
  */
object SlackMentionButton:
  /** @param slackPermalink The Slack thread permalink
    * @param mentionTextOpt Optional pre-computed @mention text (e.g., "<@U123> <@U456>")
    */
  def apply(
    slackPermalink: Option[String],
    mentionTextOpt: Option[String] = None,
  ): Option[HtmlElement] =
    slackPermalink.map { permalink =>
      button(
        cls := "SlackMentionButton",
        title := "Copy @mentions and open Slack thread",
        img(src := "/icons/slack.svg", cls := "SlackIcon"),
        span("Tag & Open"),
        onClick --> { _ =>
          // If we have mention text, copy it to clipboard first
          mentionTextOpt match
            case Some(mentionStr) if mentionStr.nonEmpty =>
              // Copy to clipboard then open Slack
              org.scalajs.dom.window.navigator.clipboard.writeText(mentionStr).toFuture.foreach { _ =>
                // Small delay to ensure clipboard write completes
                js.timers.setTimeout(100) {
                  org.scalajs.dom.window.open(permalink, "_blank")
                }
              }
            case _ =>
              // No mentions, just open Slack
              org.scalajs.dom.window.open(permalink, "_blank")
        }
      )
    }
