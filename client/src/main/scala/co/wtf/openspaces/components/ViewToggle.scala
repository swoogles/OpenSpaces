package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
// AppView is in components.ScheduleView

/** Segmented control for switching between views.
  * Modern pill-style toggle that's obvious on mobile.
  * Admin tab is only visible when admin mode is enabled.
  */
object ViewToggle:
  private def tabClass(isActive: Boolean): String =
    UiClasses.build(
      "ViewToggle-button",
      "ViewToggle-button--active" -> isActive,
    )

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
            cls <-- currentView.signal.map(view => tabClass(view == AppView.Admin)),
            onClick --> Observer(_ => currentView.set(AppView.Admin)),
            "Admin",
          )
        )
      },
      button(
        cls <-- currentView.signal.map(view => tabClass(view == AppView.Topics)),
        onClick --> Observer(_ => currentView.set(AppView.Topics)),
        "Topics",
      ),
      button(
        cls <-- currentView.signal.map(view => tabClass(view == AppView.Schedule)),
        onClick --> Observer(_ => currentView.set(AppView.Schedule)),
        "Schedule",
      ),
    )
