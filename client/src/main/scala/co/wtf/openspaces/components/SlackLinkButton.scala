package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}

/** Button component for linking/unlinking Slack account.
  * Shows "Connect Slack" when not linked, "Slack Connected" with unlink option when linked.
  */
object SlackLinkButton:
  def apply(
    username: Signal[String],
    slackLinked: Signal[Boolean],
  ): HtmlElement =
    div(
      cls := "SlackLinkButton",
      child <-- Signal.combine(username, slackLinked).map { case (user, linked) =>
        if linked then
          div(
            cls := "SlackLinkButton--linked",
            img(src := "/icons/slack.svg", cls := "SlackIcon"),
            span("Slack Connected"),
            button(
              cls := "SlackLinkButton-unlink",
              "Unlink",
              onClick --> { _ =>
                // POST to /slack/unlink endpoint
                org.scalajs.dom.fetch(
                  s"/slack/unlink?github_username=$user",
                  new org.scalajs.dom.RequestInit {
                    method = org.scalajs.dom.HttpMethod.POST
                  }
                )
                // Refresh the page to update status
                org.scalajs.dom.window.location.reload()
              }
            )
          )
        else
          a(
            href <-- username.map(u => s"/slack/auth?github_username=$u"),
            cls := "SlackLinkButton--unlinked",
            img(src := "/icons/slack.svg", cls := "SlackIcon"),
            span("Connect Slack"),
            span(cls := "SlackLinkButton-hint", "Enable @mentions"),
          )
      }
    )
