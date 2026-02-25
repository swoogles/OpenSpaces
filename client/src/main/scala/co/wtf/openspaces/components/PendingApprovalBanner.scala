package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import co.wtf.openspaces.AppState

/** Banner shown to users who are not yet approved.
  * Displays a friendly message indicating their access is pending.
  */
object PendingApprovalBanner:
  def apply(): HtmlElement =
    div(
      cls := "PendingApprovalBanner",
      display <-- AppState.authStatusLoaded.signal.combineWith(AppState.isAuthorized.signal).map {
        case (loaded, authorized) =>
          if loaded && !authorized then "flex" else "none"
      },
      div(
        cls := "PendingApprovalBanner-content",
        div(
          cls := "PendingApprovalBanner-icon",
          "⏳",
        ),
        div(
          cls := "PendingApprovalBanner-text",
          h2("Access Pending"),
          p("Your access request has been submitted. An administrator will review it shortly."),
          p(cls := "PendingApprovalBanner-subtext", "You can view the schedule and discussions, but some actions are restricted until approved."),
        ),
      ),
    )
