package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import zio.json.*

import co.wtf.openspaces.{DiscussionAction, Person, FrontEnd, connectionStatus, localStorage}

/** Admin controls component - chaos buttons, auto-schedule, delete all, and reset user.
  * 
  * Extracted from FrontEnd.scala for better code organization.
  */
object AdminControls:
  
  /** API response types */
  case class RandomActionStatus(active: Boolean) derives JsonCodec
  case class VersionInfo(version: String) derives JsonCodec
  case class ScheduleResult(scheduled: Int, moved: Int, unscheduled: Int) derives JsonCodec

  def apply(
    $showAdminControls: Signal[Boolean],
    topicUpdates: DiscussionAction => Unit,
  ): HtmlElement =
    import scala.concurrent.ExecutionContext.Implicits.global
    
    // Full chaos state
    val isChaosActive: Var[Boolean] = Var(false)
    val chaosLoading: Var[Boolean] = Var(false)
    
    // Schedule-only chaos state
    val isScheduleChaosActive: Var[Boolean] = Var(false)
    
    // Auto-schedule state
    val scheduleLoading: Var[Boolean] = Var(false)
    val scheduleResult: Var[Option[String]] = Var(None)
    val scheduleChaosLoading: Var[Boolean] = Var(false)
    
    val deleteLoading: Var[Boolean] = Var(false)
    val resetLoading: Var[Boolean] = Var(false)
    
    // Deployed version hash
    val deployedVersion: Var[String] = Var("...")
    
    // Fetch initial state on mount
    def fetchStatus(): Unit =
      // Version
      dom.fetch("/api/version")
        .toFuture
        .flatMap(_.text().toFuture)
        .foreach { text =>
          text.fromJson[VersionInfo] match
            case Right(info) => deployedVersion.set(info.version.take(7)) // Short hash
            case Left(_) => deployedVersion.set("?")
        }
      // Full chaos status
      dom.fetch("/api/admin/random-actions")
        .toFuture
        .flatMap(_.text().toFuture)
        .foreach { text =>
          text.fromJson[RandomActionStatus] match
            case Right(status) => isChaosActive.set(status.active)
            case Left(_) => ()
        }
      // Schedule chaos status
      dom.fetch("/api/admin/schedule-chaos")
        .toFuture
        .flatMap(_.text().toFuture)
        .foreach { text =>
          text.fromJson[RandomActionStatus] match
            case Right(status) => isScheduleChaosActive.set(status.active)
            case Left(_) => ()
        }
    
    def toggleChaos(): Unit =
      chaosLoading.set(true)
      dom.fetch("/api/admin/random-actions/toggle", new dom.RequestInit {
        method = dom.HttpMethod.POST
      }).toFuture
        .flatMap(_.text().toFuture)
        .foreach { text =>
          text.fromJson[RandomActionStatus] match
            case Right(status) => 
              isChaosActive.set(status.active)
              chaosLoading.set(false)
            case Left(_) => 
              chaosLoading.set(false)
        }

    def toggleScheduleChaos(): Unit =
      scheduleChaosLoading.set(true)
      dom.fetch("/api/admin/schedule-chaos/toggle", new dom.RequestInit {
        method = dom.HttpMethod.POST
      }).toFuture
        .flatMap(_.text().toFuture)
        .foreach { text =>
          text.fromJson[RandomActionStatus] match
            case Right(status) => 
              isScheduleChaosActive.set(status.active)
              scheduleChaosLoading.set(false)
            case Left(_) => 
              scheduleChaosLoading.set(false)
        }

    def runScheduling(): Unit =
      scheduleLoading.set(true)
      scheduleResult.set(None)
      dom.fetch("/api/admin/schedule", new dom.RequestInit {
        method = dom.HttpMethod.POST
      }).toFuture
        .flatMap(_.text().toFuture)
        .foreach { text =>
          scheduleLoading.set(false)
          // Show result in a toast
          text.fromJson[ScheduleResult] match
            case Right(result) =>
              val msg = s"Scheduled ${result.scheduled}, moved ${result.moved}, unscheduled ${result.unscheduled}"
              ToastManager.show(msg, "✨")
            case Left(_) =>
              ToastManager.show("Scheduling failed", "❌")
        }

    def deleteAll(): Unit =
      if dom.window.confirm("Delete ALL topics? This cannot be undone.") then
        deleteLoading.set(true)
        dom.fetch("/api/admin/topics/delete-all", new dom.RequestInit {
          method = dom.HttpMethod.POST
        }).toFuture
          .flatMap(_.text().toFuture)
          .foreach { _ =>
            deleteLoading.set(false)
          }

    def resetUser(): Unit =
      if dom.window.confirm("Reset your user? This will delete your topics, remove your votes, and reset your swipe hint.") then
        // Resilience check: ensure connection is ready before sending
        if !connectionStatus.checkReady() then
          connectionStatus.reportError("Cannot reset user while disconnected. Please wait for reconnection.")
          return
          
        resetLoading.set(true)
        val user = FrontEnd.name.now()
        
        // Send single reset action to server (handles topic deletion + vote clearing)
        topicUpdates(DiscussionAction.ResetUser(user))
        
        // Reset client-side state
        FrontEnd.everVotedTopics.set(Set.empty)
        FrontEnd.votedTopicOrder.set(Nil)
        FrontEnd.showSwipeHint.set(true)
        FrontEnd.hasSeenSwipeHint.set(false)
        localStorage.setItem("hasSeenSwipeHint", "false")
        
        resetLoading.set(false)
    
    div(
      cls := "AdminControls",
      // Only show when admin mode is active
      display <-- $showAdminControls.map(if _ then "flex" else "none"),
      onMountCallback(_ => fetchStatus()),
      // Full chaos button
      button(
        cls <-- Signal.combine(isChaosActive.signal, chaosLoading.signal).map { 
          case (_, true) => "AdminControls-button AdminControls-button--loading"
          case (true, _) => "AdminControls-button AdminControls-button--active"
          case (false, _) => "AdminControls-button"
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
        cls <-- Signal.combine(isScheduleChaosActive.signal, scheduleChaosLoading.signal).map { 
          case (_, true) => "AdminControls-button AdminControls-button--loading"
          case (true, _) => "AdminControls-button AdminControls-button--schedule-active"
          case (false, _) => "AdminControls-button"
        },
        disabled <-- scheduleChaosLoading.signal,
        onClick --> { _ => toggleScheduleChaos() },
        child.text <-- Signal.combine(isScheduleChaosActive.signal, scheduleChaosLoading.signal).map {
          case (_, true) => "⏳"
          case (true, _) => "📅 Stop Schedule Chaos"
          case (false, _) => "📅 Schedule Chaos"
        },
      ),
      // Auto-schedule button
      button(
        cls <-- scheduleLoading.signal.map { loading =>
          if loading then "AdminControls-button AdminControls-button--primary AdminControls-button--loading"
          else "AdminControls-button AdminControls-button--primary"
        },
        disabled <-- scheduleLoading.signal,
        onClick --> { _ => runScheduling() },
        child.text <-- scheduleLoading.signal.map {
          case true => "⏳"
          case false => "✨ Schedule Topics"
        },
      ),
      button(
        cls <-- deleteLoading.signal.map { loading =>
          if loading then "AdminControls-button AdminControls-button--danger AdminControls-button--loading"
          else "AdminControls-button AdminControls-button--danger"
        },
        disabled <-- deleteLoading.signal,
        onClick --> { _ => deleteAll() },
        child.text <-- deleteLoading.signal.map {
          case true => "⏳"
          case false => "🗑️ Delete All"
        },
      ),
      button(
        cls <-- resetLoading.signal.map { loading =>
          if loading then "AdminControls-button AdminControls-button--warning AdminControls-button--loading"
          else "AdminControls-button AdminControls-button--warning"
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
