package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import neotype.unwrap
import org.scalajs.dom
import co.wtf.openspaces.{AppState, ConnectionState, Person, GitHubAvatar}
import co.wtf.openspaces.ConnectionStatusIndicator
import co.wtf.openspaces.services.{AuthService, GeolocationService, NotificationService}
import co.wtf.openspaces.location.LocationAction

object NameBadge:
  def apply(
    name: Var[Person],
    connectionState: Signal[ConnectionState],
    soundMuted: Var[Boolean],
    sendLocationAction: LocationAction => Unit,
  ) =
    div(
      cls := "Banner",
      img(cls := "LogoImg",
          src := "./wtf-web-nodate.jpg",
          role := "img",
      ),
      div(
        cls := "UserProfileSection",
        // Slack link button - prominent call to action for unlinked users
        SlackLinkButton(name.signal.map(_.unwrap), AppState.slackLinked.signal),
        // Location sharing toggle button
        button(
          cls := "location-toggle",
          cls <-- AppState.locationSharingEnabled.signal.map { sharing =>
            if sharing then "location-toggle--active" else ""
          },
          title <-- AppState.locationSharingEnabled.signal.combineWith(AppState.sharersCount).map {
            case (sharing, count) =>
              if sharing then s"Sharing your location (${count} people sharing)"
              else s"Share your location (${count} people sharing)"
          },
          aria.label <-- AppState.locationSharingEnabled.signal.map { sharing =>
            if sharing then "Stop sharing location" else "Share your location"
          },
          child.text <-- AppState.locationSharingEnabled.signal.map { sharing =>
            if sharing then "📍" else "📌"
          },
          onClick --> Observer { _ =>
            if GeolocationService.isAvailable then
              GeolocationService.toggleSharing(
                sendLocationAction,
                // On stop, send StopSharing action
                sendLocationAction(LocationAction.StopSharing(AppState.name.now()))
              )
            else
              dom.window.alert("Location services are not available in this browser.")
          },
        ),
        // Sound toggle button
        button(
          cls := "sound-toggle",
          cls <-- soundMuted.signal.map { muted =>
            if muted then "sound-toggle--muted" else ""
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

object LoadingPreviewToggle:
  import org.scalajs.dom
  
  def apply(
    $isAdmin: Signal[Boolean],
    showLoadingPreview: Var[Boolean],
  ) =
    div(
      cls := "AdminModeToggle", // Reuse same styling
      // Only show for admins
      display <-- $isAdmin.map(if _ then "flex" else "none"),
      label(
        cls := "AdminModeToggle-label",
        input(
          typ := "checkbox",
          checked <-- showLoadingPreview.signal,
          onChange.mapToChecked --> { checked =>
            showLoadingPreview.set(checked)
            // Toggle the loading screen visibility
            Option(dom.document.getElementById("loading-screen")).foreach { el =>
              if checked then el.classList.remove("hidden")
              else el.classList.add("hidden")
            }
          },
        ),
        span("Loading Preview"),
      ),
    )
