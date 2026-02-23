package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

import co.wtf.openspaces.{Person, SafeStorage}
import co.wtf.openspaces.discussions.DiscussionAction
import co.wtf.openspaces.AppState
import co.wtf.openspaces.*
import io.laminext.websocket.*
import scala.util.{Failure, Success}

/** Admin controls component - chaos buttons, auto-schedule, delete all, and reset user.
  * 
  * Extracted from AppState.scala for better code organization.
  */
object AdminControls:
  def apply(
    $showAdminControls: Signal[Boolean],
    topicUpdates: DiscussionAction => Unit,
    connectionStatus: ConnectionStatusUI,
    randomActionClient: RandomActionClient,
    openReplayView: Observer[Unit],
  ): HtmlElement =
    import scala.concurrent.ExecutionContext.Implicits.global
    
    // Full chaos state
    val isChaosActive: Var[Boolean] = Var(false)
    val chaosLoading: Var[Boolean] = Var(false)
    
    // Schedule-only chaos state
    val isScheduleChaosActive: Var[Boolean] = Var(false)
    
    // Hackathon chaos state
    val isHackathonChaosActive: Var[Boolean] = Var(false)
    val hackathonChaosLoading: Var[Boolean] = Var(false)
    
    // Auto-schedule state
    val scheduleLoading: Var[Boolean] = Var(false)
    val scheduleChaosLoading: Var[Boolean] = Var(false)
    
    val deleteLoading: Var[Boolean] = Var(false)
    val resetLoading: Var[Boolean] = Var(false)
    
    // Deployed version hash
    val deployedVersion: Var[String] = Var("...")
    
    // Fetch initial state on mount
    def fetchStatus(): Unit =
      // Version
      randomActionClient.version
        .onComplete {
          case Success(info) => deployedVersion.set(info.version.take(7)) // Short hash
          case Failure(_) => deployedVersion.set("?")
        }
      // Full chaos status
      randomActionClient.randomActionStatus
        .foreach(status => isChaosActive.set(status.active))
      // Schedule chaos status
      randomActionClient.randomScheduleStatus
        .foreach(status => isScheduleChaosActive.set(status.active))
      // Hackathon chaos status
      randomActionClient.hackathonChaosStatus
        .foreach(status => isHackathonChaosActive.set(status.active))
    
    def toggleChaos(): Unit = {
      chaosLoading.set(true)
      
      randomActionClient.randomActionToggle
        .onComplete {
          case Success(status) =>
            isChaosActive.set(status.active)
            chaosLoading.set(false)
          case Failure(_) =>
            chaosLoading.set(false)
        }
    }

    def toggleScheduleChaos(): Unit = {
      scheduleChaosLoading.set(true)
      randomActionClient.randomScheduleActionToggle
        .onComplete {
          case Success(status) =>
            isScheduleChaosActive.set(status.active)
            scheduleChaosLoading.set(false)
          case Failure(_) =>
            scheduleChaosLoading.set(false)
        }
      }

    def toggleHackathonChaos(): Unit = {
      hackathonChaosLoading.set(true)
      randomActionClient.hackathonChaosToggle
        .onComplete {
          case Success(status) =>
            isHackathonChaosActive.set(status.active)
            hackathonChaosLoading.set(false)
          case Failure(_) =>
            hackathonChaosLoading.set(false)
        }
    }

    def runScheduling(): Unit =
      scheduleLoading.set(true)
      randomActionClient.runScheduling
        .onComplete {
          case Success(result) =>
            scheduleLoading.set(false)
            val msg = s"Scheduled ${result.scheduled}, moved ${result.moved}, unscheduled ${result.unscheduled}"
            ToastManager.show(msg, "✨")
          case Failure(_) =>
            scheduleLoading.set(false)
            ToastManager.show("Scheduling failed", "❌")
        }

    def deleteAll(): Unit =
      if dom.window.confirm("Delete ALL topics? This cannot be undone.") then
        deleteLoading.set(true)
        randomActionClient.deleteAllTopics
          .onComplete { _ =>
            deleteLoading.set(false)
          }

    def resetUser(): Unit =
      if dom.window.confirm("Reset your user? This will delete your topics, remove your votes, and reset your swipe hint.") then
        // Resilience check: ensure connection is ready before sending
        if !connectionStatus.checkReady() then
          connectionStatus.reportError("Cannot reset user while disconnected. Please wait for reconnection.")
          return
          
        resetLoading.set(true)
        val user = AppState.name.now()
        
        // Send single reset action to server (handles topic deletion + vote clearing)
        topicUpdates(DiscussionAction.ResetUser(user))
        
        // Reset client-side state
        AppState.showSwipeHint.set(true)
        AppState.hasSeenSwipeHint.set(false)
        SafeStorage.setItem("hasSeenSwipeHint", "false")
        
        resetLoading.set(false)
    
    div(
      cls := "AdminControls",
      // Only show when admin mode is active
      display <-- $showAdminControls.map(if _ then "flex" else "none"),
      onMountCallback(_ => fetchStatus()),
      // Full chaos button
      button(
        cls := "AdminControls-button",
        cls <-- Signal.combine(isChaosActive.signal, chaosLoading.signal).map {
          case (isActive, _) =>
            if isActive then "AdminControls-button--active" else ""
        },
        cls <-- chaosLoading.signal.map { isLoading =>
          if isLoading then "AdminControls-button--loading" else ""
        },
        disabled <-- chaosLoading.signal,
        onClick --> { _ => toggleChaos() },
        child.text <-- Signal.combine(isChaosActive.signal, chaosLoading.signal).map {
          case (_, true) => "⏳"
          case (true, _) => "🎲 Stop Chaos"
          case (false, _) => "🎲 Start Chaos"
        },
      ),
      // Schedule-only chaos button
      button(
        cls := "AdminControls-button",
        cls <-- Signal.combine(isScheduleChaosActive.signal, scheduleChaosLoading.signal).map {
          case (isActive, _) =>
            if isActive then "AdminControls-button--schedule-active" else ""
        },
        cls <-- scheduleChaosLoading.signal.map { isLoading =>
          if isLoading then "AdminControls-button--loading" else ""
        },
        disabled <-- scheduleChaosLoading.signal,
        onClick --> { _ => toggleScheduleChaos() },
        child.text <-- Signal.combine(isScheduleChaosActive.signal, scheduleChaosLoading.signal).map {
          case (_, true) => "⏳"
          case (true, _) => "📅 Stop Sched"
          case (false, _) => "📅 Sched"
        },
      ),
      // Hackathon chaos button
      button(
        cls := "AdminControls-button",
        cls <-- Signal.combine(isHackathonChaosActive.signal, hackathonChaosLoading.signal).map {
          case (isActive, _) =>
            if isActive then "AdminControls-button--hackathon-active" else ""
        },
        cls <-- hackathonChaosLoading.signal.map { isLoading =>
          if isLoading then "AdminControls-button--loading" else ""
        },
        disabled <-- hackathonChaosLoading.signal,
        onClick --> { _ => toggleHackathonChaos() },
        child.text <-- Signal.combine(isHackathonChaosActive.signal, hackathonChaosLoading.signal).map {
          case (_, true) => "⏳"
          case (true, _) => "🛠️ Stop Hack"
          case (false, _) => "🛠️ Hack"
        },
      ),
      // Auto-schedule button
      button(
        cls := "AdminControls-button",
        cls := "AdminControls-button--primary",
        onClick.mapToUnit --> openReplayView,
        "▶ Replay",
      ),
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
      button(
        cls := "AdminControls-button",
        cls := "AdminControls-button--danger",
        cls <-- deleteLoading.signal.map { loading =>
          if loading then "AdminControls-button--loading" else ""
        },
        disabled <-- deleteLoading.signal,
        onClick --> { _ => deleteAll() },
        child.text <-- deleteLoading.signal.map {
          case true => "⏳"
          case false => "🗑️ Delete All"
        },
      ),
      button(
        cls := "AdminControls-button",
        cls := "AdminControls-button--warning",
        cls <-- resetLoading.signal.map { loading =>
          if loading then "AdminControls-button--loading" else ""
        },
        disabled <-- resetLoading.signal,
        onClick --> { _ => resetUser() },
        child.text <-- resetLoading.signal.map {
          case true => "⏳"
          case false => "🔄 Reset User"
        },
      ),
      // Version display
      span(
        cls := "AdminControls-version",
        child.text <-- deployedVersion.signal.map(v => s"v$v"),
      ),
    )
