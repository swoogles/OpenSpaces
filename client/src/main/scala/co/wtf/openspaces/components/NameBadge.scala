package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import co.wtf.openspaces.{ConnectionState, Person, GitHubAvatar}
import co.wtf.openspaces.ConnectionStatusIndicator
import co.wtf.openspaces.services.AuthService

object NameBadge:
  def apply(
    name: Var[Person],
    connectionState: Signal[ConnectionState],
    soundMuted: Var[Boolean],
  ) =
    div(
      cls := "Banner",
      img(cls := "LogoImg",
          src := "./wtf-web-nodate.jpg",
          role := "img",
      ),
      div(
        cls := "UserProfileSection",
        // Sound toggle button
        button(
          cls <-- soundMuted.signal.map { muted =>
            UiClasses.build(
              "sound-toggle",
              "sound-toggle--muted" -> muted,
            )
          },
          title <-- soundMuted.signal.map { muted =>
            if muted then "Sound off - click to enable"
            else "Sound on - click to mute"
          },
          aria.label <-- soundMuted.signal.map { muted =>
            if muted then "Enable sound effects"
            else "Mute sound effects"
          },
          child.text <-- soundMuted.signal.map { muted =>
            if muted then "🔇" else "🔊"
          },
          onClick --> Observer { _ =>
            soundMuted.update(!_)
          },
        ),
        // Connection status indicator dot
        ConnectionStatusIndicator.dot(connectionState),
        // Profile icon - click to logout
        span(
          cls := "ProfileIconButton",
          title := "Click to logout",
          child <-- name.signal.map { person =>
            GitHubAvatar(person, "github-avatar")
          },
          onClick --> Observer { _ =>
            if (dom.window.confirm("Log out?")) {
              AuthService.deleteCookie("access_token")
              AuthService.deleteCookie("access_token_expires_at")
              AuthService.deleteCookie("github_username")
              dom.window.location.reload()
            }
          },
        ),
      ),
    )

  /** Banner logo only (for simpler contexts) */
  def logo() =
    div(width := "100%",
        img(cls := "LogoImg",
            src := "./wtf-web-nodate.jpg",
            role := "img",
        ),
    )

/** Admin mode toggle - shows at very top, only for admins */
object AdminModeToggle:
  def apply(
    $isAdmin: Signal[Boolean],
    adminModeEnabled: Var[Boolean],
  ) =
    div(
      cls := "AdminModeToggle",
      // Only show for admins
      display <-- $isAdmin.map(if _ then "flex" else "none"),
      label(
        cls := "AdminModeToggle-label",
        input(
          typ := "checkbox",
          checked <-- adminModeEnabled.signal,
          onChange.mapToChecked --> adminModeEnabled,
        ),
        span("Admin Mode"),
      ),
    )
