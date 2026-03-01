package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

import co.wtf.openspaces.AppState
import co.wtf.openspaces.RandomActionClient
import scala.util.{Failure, Success}

/** Admin controls component - safe admin actions with confirmation dialogs.
  * 
  * Dangerous chaos/test buttons have been removed for production safety.
  * Destructive actions require typing a confirmation phrase.
  */
object AdminControls:
  def apply(
    $showAdminControls: Signal[Boolean],
    randomActionClient: RandomActionClient,
  ): HtmlElement =
    import scala.concurrent.ExecutionContext.Implicits.global
    
    // Auto-schedule state
    val scheduleLoading: Var[Boolean] = Var(false)
    val clearScheduleLoading: Var[Boolean] = Var(false)
    val reloadLoading: Var[Boolean] = Var(false)
    val scheduleSummary: Var[String] = Var("")
    
    // Confirmation dialog state
    val showClearConfirm: Var[Boolean] = Var(false)
    val clearConfirmInput: Var[String] = Var("")
    
    // Deployed version hash
    val deployedVersion: Var[String] = Var("...")
    
    // Fetch initial state on mount
    def fetchStatus(): Unit =
      randomActionClient.version
        .onComplete {
          case Success(info) => deployedVersion.set(info.version.take(7))
          case Failure(_) => deployedVersion.set("?")
        }

    def runScheduling(): Unit =
      scheduleLoading.set(true)
      randomActionClient.runScheduling
        .onComplete {
          case Success(result) =>
            scheduleSummary.set(
              s"Sched ${result.scheduled} • Move ${result.moved} • Unsched ${result.unscheduled} • Locked ${result.lockedExcluded}",
            )
            scheduleLoading.set(false)
          case Failure(_) =>
            scheduleSummary.set("Scheduling failed.")
            scheduleLoading.set(false)
        }

    def clearSchedule(): Unit =
      clearScheduleLoading.set(true)
      randomActionClient.clearSchedule
        .onComplete {
          case Success(result) =>
            scheduleSummary.set(s"Cleared ${result.cleared} topics from schedule")
            clearScheduleLoading.set(false)
            showClearConfirm.set(false)
            clearConfirmInput.set("")
          case Failure(_) =>
            scheduleSummary.set("Clear schedule failed.")
            clearScheduleLoading.set(false)
        }

    def reloadStateFromDatabase(): Unit =
      reloadLoading.set(true)
      randomActionClient.reloadState
        .onComplete {
          case Success(result) =>
            scheduleSummary.set(
              s"Reloaded DB state: ${result.discussions} discussions • ${result.lightningTalks} lightning • ${result.hackathonProjects} hackathon • ${result.activities} activities",
            )
            reloadLoading.set(false)
          case Failure(_) =>
            scheduleSummary.set("Reload from DB failed.")
            reloadLoading.set(false)
        }

    // Confirmation dialog component
    def confirmationDialog(
      show: Var[Boolean],
      inputVar: Var[String],
      requiredPhrase: String,
      title: String,
      description: String,
      onConfirm: () => Unit,
      loading: Signal[Boolean],
    ): HtmlElement =
      div(
        cls := "AdminControls-confirmOverlay",
        display <-- show.signal.map(if _ then "flex" else "none"),
        onClick --> { _ => show.set(false) },
        div(
          cls := "AdminControls-confirmDialog",
          onClick.stopPropagation --> { _ => () },
          h3(title),
          p(description),
          p(cls := "AdminControls-confirmHint", s"Type \"$requiredPhrase\" to confirm:"),
          input(
            cls := "AdminControls-confirmInput",
            typ := "text",
            placeholder := requiredPhrase,
            controlled(
              value <-- inputVar.signal,
              onInput.mapToValue --> inputVar.writer,
            ),
          ),
          div(
            cls := "AdminControls-confirmButtons",
            button(
              cls := "AdminControls-button AdminControls-button--secondary",
              "Cancel",
              onClick --> { _ => 
                show.set(false)
                inputVar.set("")
              },
            ),
            button(
              cls := "AdminControls-button AdminControls-button--danger",
              disabled <-- Signal.combine(inputVar.signal, loading).map { case (input, isLoading) =>
                input.toLowerCase.trim != requiredPhrase.toLowerCase || isLoading
              },
              child.text <-- loading.map(if _ then "⏳" else "Confirm"),
              onClick --> { _ => onConfirm() },
            ),
          ),
        ),
      )
    
    div(
      cls := "AdminControls",
      display <-- $showAdminControls.map(if _ then "flex" else "none"),
      onMountCallback(_ => fetchStatus()),
      
      // Auto-schedule button
      button(
        cls := "AdminControls-button",
        cls := "AdminControls-button--primary",
        cls <-- scheduleLoading.signal.map { loading =>
          if loading then "AdminControls-button--loading" else ""
        },
        disabled <-- scheduleLoading.signal,
        onClick --> { _ => runScheduling() },
        child.text <-- scheduleLoading.signal.map {
          case true => "⏳"
          case false => "✨ Schedule Topics"
        },
      ),
      
      // Clear Schedule button (opens confirmation dialog)
      button(
        cls := "AdminControls-button",
        cls := "AdminControls-button--warning",
        onClick --> { _ => showClearConfirm.set(true) },
        "🧹 Clear Schedule",
      ),
      
      // Reload DB button
      button(
        cls := "AdminControls-button",
        cls := "AdminControls-button--primary",
        cls <-- reloadLoading.signal.map { loading =>
          if loading then "AdminControls-button--loading" else ""
        },
        disabled <-- reloadLoading.signal,
        onClick --> { _ => reloadStateFromDatabase() },
        child.text <-- reloadLoading.signal.map {
          case true => "⏳"
          case false => "↻ Reload DB"
        },
      ),
      
      // User Management button
      button(
        cls := "AdminControls-button",
        cls := "AdminControls-button--primary",
        "👥 Users",
        onClick --> { _ => AppState.showUserManagement.set(true) },
      ),
      
      // Version display
      span(
        cls := "AdminControls-version",
        child.text <-- deployedVersion.signal.map(v => s"v$v"),
      ),
      span(
        cls := "AdminControls-version",
        child.text <-- scheduleSummary.signal,
      ),
      
      // Confirmation dialog
      confirmationDialog(
        show = showClearConfirm,
        inputVar = clearConfirmInput,
        requiredPhrase = "unschedule all",
        title = "⚠️ Clear Entire Schedule",
        description = "This will unschedule all topics. Topics will not be deleted, but all room/time assignments will be cleared.",
        onConfirm = () => clearSchedule(),
        loading = clearScheduleLoading.signal,
      ),
    )
