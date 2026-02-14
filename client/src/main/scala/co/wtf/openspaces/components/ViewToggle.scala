package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import co.wtf.openspaces.AppView

/** Segmented control for switching between views.
  * Modern pill-style toggle that's obvious on mobile.
  * Admin tab is only visible when admin mode is enabled.
  */
object ViewToggle:
  def apply(
    currentView: Var[AppView],
    adminModeEnabled: Signal[Boolean],
  ) =
    div(
      cls := "ViewToggle",
      // Admin button - only when admin mode is enabled
      child.maybe <-- adminModeEnabled.map { enabled =>
        Option.when(enabled)(
          button(
            cls <-- currentView.signal.map { view =>
              if (view == AppView.Admin) "ViewToggle-button ViewToggle-button--active"
              else "ViewToggle-button"
            },
            onClick --> Observer(_ => currentView.set(AppView.Admin)),
            "Admin",
          )
        )
      },
      button(
        cls <-- currentView.signal.map { view =>
          if (view == AppView.Topics) "ViewToggle-button ViewToggle-button--active"
          else "ViewToggle-button"
        },
        onClick --> Observer(_ => currentView.set(AppView.Topics)),
        "Topics",
      ),
      button(
        cls <-- currentView.signal.map { view =>
          if (view == AppView.Schedule) "ViewToggle-button ViewToggle-button--active"
          else "ViewToggle-button"
        },
        onClick --> Observer(_ => currentView.set(AppView.Schedule)),
        "Schedule",
      ),
    )
