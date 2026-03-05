package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import co.wtf.openspaces.AppState
// AppView is defined in ScheduleView.scala in the same package

/** Bottom navigation bar with view tabs.
  * Fixed to bottom of screen for thumb-friendly mobile use.
  * Shows badge on Topics tab when there are unvoted topics.
  * Admin controls integrated when admin mode is enabled.
  */
object ViewToggle:
  def apply(
    currentView: Var[AppView],
    adminModeEnabled: Signal[Boolean],
  ) =
    val unjudgedCount = AppState.unjudgedTopicCount
    val isAdmin = AppState.isAdmin

    div(
      cls := "BottomNav",
      // Admin button - only when admin mode is enabled
      child.maybe <-- adminModeEnabled.map { enabled =>
        Option.when(enabled)(
          button(
            cls := "BottomNav-button",
            cls <-- currentView.signal.map { view =>
              if view == AppView.Admin then "BottomNav-button--active" else ""
            },
            onClick --> Observer(_ => currentView.set(AppView.Admin)),
            span(cls := "BottomNav-icon", "⚙"),
            span(cls := "BottomNav-label", "Admin"),
          )
        )
      },
      button(
        cls := "BottomNav-button",
        cls <-- currentView.signal.map { view =>
          if view == AppView.Topics then "BottomNav-button--active" else ""
        },
        onClick --> Observer(_ => currentView.set(AppView.Topics)),
        span(
          cls := "BottomNav-iconWrapper",
          span(cls := "BottomNav-icon", "💬"),
          // Badge for unvoted topics
          child.maybe <-- unjudgedCount.map { count =>
            Option.when(count > 0)(
              span(
                cls := "BottomNav-badge",
                if count > 99 then "99+" else count.toString,
              )
            )
          },
        ),
        span(cls := "BottomNav-label", "Topics"),
      ),
      button(
        cls := "BottomNav-button",
        cls <-- currentView.signal.map { view =>
          if view == AppView.LightningTalks then "BottomNav-button--active" else ""
        },
        onClick --> Observer(_ => currentView.set(AppView.LightningTalks)),
        span(cls := "BottomNav-icon", "🎤"),
        span(cls := "BottomNav-label", "Activities"),
      ),
      button(
        cls := "BottomNav-button",
        cls <-- currentView.signal.map { view =>
          if view == AppView.Hackathon then "BottomNav-button--active" else ""
        },
        onClick --> Observer(_ => currentView.set(AppView.Hackathon)),
        span(cls := "BottomNav-icon", "🛠"),
        span(cls := "BottomNav-label", "Hack"),
      ),
      button(
        cls := "BottomNav-button",
        cls <-- currentView.signal.map { view =>
          if view == AppView.Schedule then "BottomNav-button--active" else ""
        },
        onClick --> Observer(_ => currentView.set(AppView.Schedule)),
        span(cls := "BottomNav-icon", "📅"),
        span(cls := "BottomNav-label", "Schedule"),
      ),
    )
