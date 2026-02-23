package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
// AppView is defined in ScheduleView.scala in the same package

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
            cls := "open-spaces-button",
            cls := "ViewToggle-button",
            cls <-- currentView.signal.map { view =>
              if view == AppView.Admin then "ViewToggle-button--active" else ""
            },
            onClick --> Observer(_ => currentView.set(AppView.Admin)),
            "Admin",
          )
        )
      },
      button(
        cls := "open-spaces-button",
        cls := "ViewToggle-button",
        cls <-- currentView.signal.map { view =>
          if view == AppView.Topics then "ViewToggle-button--active" else ""
        },
        onClick --> Observer(_ => currentView.set(AppView.Topics)),
        "Topics",
      ),
      button(
        cls := "open-spaces-button",
        cls := "ViewToggle-button",
        cls <-- currentView.signal.map { view =>
          if view == AppView.LightningTalks then "ViewToggle-button--active" else ""
        },
        onClick --> Observer(_ => currentView.set(AppView.LightningTalks)),
        "Lightning",
      ),
      button(
        cls := "open-spaces-button",
        cls := "ViewToggle-button",
        cls <-- currentView.signal.map { view =>
          if view == AppView.Hackathon then "ViewToggle-button--active" else ""
        },
        onClick --> Observer(_ => currentView.set(AppView.Hackathon)),
        "Hack",
      ),
      button(
        cls := "open-spaces-button",
        cls := "ViewToggle-button",
        cls <-- currentView.signal.map { view =>
          if view == AppView.Schedule then "ViewToggle-button--active" else ""
        },
        onClick --> Observer(_ => currentView.set(AppView.Schedule)),
        "Sched",
      ),
    )
