package co.wtf.openspaces

import animus.*
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import org.scalajs.dom.window
import zio.json.*
import zio.*
import neotype.*

import co.wtf.openspaces.discussions.{DiscussionState, DiscussionAction, DiscussionActionConfirmed}
import co.wtf.openspaces.hackathon.*

// Import extracted utilities
import co.wtf.openspaces.util.{SlotPositionTracker, SwapAnimationState, MenuPositioning, ScrollPreserver}
import co.wtf.openspaces.components.{AdminControls, ErrorBanner, ViewToggle, NameBadge, AdminModeToggle, LoadingPreviewToggle, Menu, UnscheduledDiscussionsMenu, ActiveDiscussionActionMenu, ScheduleView, LinearScheduleView, ReplayView, AppView}
import co.wtf.openspaces.components.discussions.DiscussionSubview
import co.wtf.openspaces.components.discussions.TopicSubmission
import co.wtf.openspaces.components.hackathon.HackathonProjectsView
import co.wtf.openspaces.components.activities.ActivitiesView
import co.wtf.openspaces.AppState.*
import co.wtf.openspaces.*
import co.wtf.openspaces.services.{AudioService, AuthService}
import zio.http.endpoint.{Endpoint, EndpointExecutor}
import co.wtf.openspaces.lighting_talks.*
import co.wtf.openspaces.activities.*
import co.wtf.openspaces.discussions.Discussion
import co.wtf.openspaces.discussions.VotePosition

/** FrontEnd.scala - Main entry point and app composition
  * 
  * Utility objects have been extracted to:
  * - util/SlotPositionTracker.scala
  * - util/SwapAnimationState.scala  
  * - util/MenuPositioning.scala
  * - util/ScrollPreserver.scala
  */

object FrontEnd extends ZIOAppDefault{
  import org.scalajs.dom
  import org.scalajs.dom.window
  
  override val bootstrap: ZLayer[Any, Nothing, Unit] =
    Runtime.setConfigProvider(
      ConfigProvider.fromMap(
        Map(
        "open-spaces.url" ->  
        (if (window.location.hostname == "localhost") 
          "http://localhost:8080"
        else
          "https://app.wintertechforum.com"
          )
          )
        )
        )

  // ZIO.config(Config.config.nested(serviceName))
  def run =  ZIO.service[EndpointExecutor[Any, Unit, Scope]].map{ executor => 
  ServiceWorkerClient.registerServiceWorker()
  lazy val container = dom.document.getElementById("app")

  // App state moved to AppState.scala
  val discussionState = AppState.discussionState
  val lightningTalkState = AppState.lightningTalkState
  val hackathonProjectState = AppState.hackathonProjectState
  val activityState = AppState.activityState
  val soundMuted = AppState.soundMuted
  val celebratingTopics = AppState.celebratingTopics
  val hasSeenSwipeHint = AppState.hasSeenSwipeHint
  val showSwipeHint = AppState.showSwipeHint
  val activeDiscussion = AppState.activeDiscussion
  val popoverState = AppState.popoverState
  val swapMenuState = AppState.swapMenuState
  val unscheduledMenuState = AppState.unscheduledMenuState
  val activeDiscussionMenuState = AppState.activeDiscussionMenuState
  val currentAppView = AppState.currentAppView
  val name = AppState.name
  val isAdmin = AppState.isAdmin
  val adminModeEnabled = AppState.adminModeEnabled

  def dismissSwipeHint(): Unit = AppState.dismissSwipeHint()

  def initAudioOnGesture(): Unit = AudioService.initAudioOnGesture()
  def playVoteSound(position: VotePosition): Unit = AudioService.playVoteSound(position)
  def celebrateVote(topicId: TopicId, position: VotePosition): Unit = AudioService.celebrateVote(topicId, position)

  val errorBanner =
    ErrorBanner(connectionStatus)

  val sendDiscussionAction: DiscussionAction => Unit = action =>
    connectionStatus.withReady("Syncing latest topics. Please wait a moment.") {
      topicUpdates.sendOne(DiscussionActionMessage(action))
    }

  val sendLightningAction: LightningTalkAction => Unit = action =>
    connectionStatus.withReady("Syncing latest lightning talks. Please wait a moment.") {
      topicUpdates.sendOne(LightningTalkActionMessage(action))
    }

  val sendHackathonAction: HackathonProjectAction => Unit = action =>
    connectionStatus.withReady("Syncing latest hackathon projects. Please wait a moment.") {
      topicUpdates.sendOne(HackathonProjectActionMessage(action))
    }

  val sendActivityAction: ActivityAction => Unit = action =>
    connectionStatus.withReady("Syncing latest activities. Please wait a moment.") {
      topicUpdates.sendOne(ActivityActionMessage(action))
    }

  val submitNewTopic: Observer[DiscussionAction] = Observer {
    case discussion @ (add: DiscussionAction.Add) =>
      if (add.facilitator.unwrap.trim.length < 2)
        errorBanner.setError("User name too short. Tell us who you are!")
      else
        errorBanner.clearError()
        sendDiscussionAction(discussion)
    case _ => ()
  }

  def liveTopicSubmissionAndVoting(
    updateTargetDiscussion: Observer[Discussion],
  ) =
    val $orderedTopics = Signal.combine(
      discussionState.signal,
      name.signal,
    ).map { case (state, currentUser) =>
      state.data.values.toList.sortBy(topic => (topic.createdAtEpochMs, topic.id.unwrap))
    }

    val $unjudgedTopics = Signal.combine(
      $orderedTopics,
      name.signal,
    ).map { case (topics, currentUser) =>
      topics.filter(topic => !topic.interestedParties.exists(_.voter == currentUser))
    }

    val $votedTopics = Signal.combine(
      $orderedTopics,
      name.signal,
    ).map { case (topics, currentUser) =>
      topics
        .filter(topic => topic.interestedParties.exists(_.voter == currentUser))
        .sortBy { topic =>
          val firstVoteTs = topic.interestedParties
            .find(_.voter == currentUser)
            .flatMap(_.firstVotedAtEpochMs)
            .getOrElse(Long.MinValue)
          (-firstVoteTs, topic.id.unwrap)
        }
    }

    val $nextUnjudgedTopic = $unjudgedTopics.map(_.headOption.toList)

    // Signal for counting unjudged topics
    val $unjudgedCount = $unjudgedTopics.map(_.size)

    // Track the first unjudged topic ID for swipe hint
    val $firstUnjudgedId: Signal[Option[TopicId]] =
      $nextUnjudgedTopic.map(_.headOption.map(_.id))

    val $hasVotedTopics = $votedTopics.map(_.nonEmpty)
    val $hasNextTopic = $nextUnjudgedTopic.map(_.nonEmpty)

    div(
      TopicSubmission(submitNewTopic,
                      name.signal,
                      connectionStatus,
      ),
      // Counter showing remaining topics to vote on
      div(
        cls := "TopicGroups",
        div(
          cls := "TopicSection",
          h3(cls := "TopicSection-title", 
          child.text <-- $unjudgedCount.map { count =>
            if count == 0 then "Thank you for giving feedback on everything! Check back later for more topics!"
            else if count == 1 then "1 topic needs your feedback!"
            else s"$count topics need your feedback!"
          },
          ),
          child <-- $unjudgedCount.map { count =>
            if (count == 0) p()
            else  p(
              cls := "TopicSection-subtitle",
              "Swipe to Vote. Vote to reveal the next. Swiping will not hurt feelings or data integrity. Everything can be remedied.",
            )
          },
          DiscussionSubview(
            $nextUnjudgedTopic,
            None,
            name.signal,
            isAdmin,
            sendDiscussionAction,
            updateTargetDiscussion,
            connectionStatus,
            $firstUnjudgedId,
            showSwipeHint.signal,
          ),
        ),
        div(
          cls := "TopicSection",
          h3(cls := "TopicSection-title", "Viewed Topics"),
          child.maybe <-- $hasVotedTopics.map { hasVoted =>
            if hasVoted then None
            else Some(div(cls := "TopicSection-empty", "No voted topics yet."))
          },
          DiscussionSubview(
            $votedTopics,
            None,
            name.signal,
            isAdmin,
            sendDiscussionAction,
            updateTargetDiscussion,
            connectionStatus,
          ),
        ),
      ),
    )

  val updateTargetDiscussion: Observer[Discussion] =
    Observer[Discussion] { discussion =>
      dom.document
        .getElementsByClassName("ActiveDiscussion")
        .head
        .scrollIntoView(
          top = false,
        )
      activeDiscussion.set(Some(discussion))
    }

  val setActiveDiscussion: Observer[Discussion] = Observer {
    discussion =>
      val expectedCurrentRoomSlot =
        discussionState.now().data.get(discussion.id).flatMap(_.roomSlot)
      sendDiscussionAction(
        DiscussionAction.SetRoomSlot(
          discussion.id,
          expectedCurrentRoomSlot,
          discussion.roomSlot,
        ),
      )

      activeDiscussion.set(Some(discussion))
  }

  val dismissSwapMenu: Observer[Unit] =
    Observer { _ =>
      swapMenuState.set(None)
    }

  val dismissUnscheduledMenu: Observer[Unit] =
    Observer { _ =>
      unscheduledMenuState.set(None)
    }

  val dismissActiveDiscussionMenu: Observer[Unit] =
    Observer { _ =>
      activeDiscussionMenuState.set(None)
    }

  val hasAuthCookies =
    AuthService.getCookie("access_token").isDefined ||
      AuthService.getCookie("access_token_expires_at").isDefined ||
      AuthService.getCookie("github_username").isDefined
  val randomActionClient = RandomActionClient(executor)

  val app =
    div(
      cls := "PageContainer",
      // Hide loading screen when app mounts
      onMountCallback { _ =>
        Option(dom.document.getElementById("loading-screen")).foreach { el =>
          el.classList.add("hidden")
        }
      },
      // Initialize audio on first user interaction (required by browser autoplay policy)
      onClick --> Observer(_ => initAudioOnGesture()),
      onTouchStart --> Observer(_ => initAudioOnGesture()),
      onMouseDown --> Observer(_ => initAudioOnGesture()),
      // Conditional connect binder - toggles off/on to force reconnection
      // Uses connectionStatus.connectionEnabled which is controlled by the manager
      child <-- connectionStatus.connectionEnabled.signal.map { enabled =>
        if enabled then
          div(topicUpdates.connect)
        else
          div() // Disconnected state - binder unmounted
      },
      // Connection status monitoring
      connectionStatus.bind,
      topicUpdates.closed --> connectionStatus.closeObserver,
      topicUpdates.connected --> connectionStatus.connectedObserver,
      // Connection status banner (shows when disconnected/reconnecting/syncing)
      ConnectionStatusBanner.withSyncStatus(
        connectionStatus.state,
        connectionStatus.syncMessage,
        Observer(_ => connectionStatus.forceReconnect()),
      ),
      // Popover component at top level
      // Swap action menu at top level
      child <-- swapMenuState.signal.map {
        case Some((selectedDiscussion, targetDiscussion)) =>
          Menu(
            selectedDiscussion,
            targetDiscussion,
            sendDiscussionAction,
            dismissSwapMenu,
          )
        case None =>
          div()
      },
      // Unscheduled discussions menu at top level
      child <-- unscheduledMenuState.signal
        .combineWith(discussionState.signal)
        .combineWith(activeDiscussion.signal)
        .combineWith(currentAppView.signal)
        .map {
          case (Some(roomSlot), discState, activeDiscussionOpt, view) =>
            val unscheduledDiscussions = discState.data.values
              .filter(_.roomSlot.isEmpty)
              .toList
            UnscheduledDiscussionsMenu(
              unscheduledDiscussions,
              roomSlot,
              name.signal,
              sendDiscussionAction,
              dismissUnscheduledMenu,
              setActiveDiscussion,
              activeDiscussionOpt,
              view,
            )
          case _ =>
            div()
        },
      // Active discussion action menu at top level
      child <-- activeDiscussionMenuState.signal.map {
        case Some(discussion) =>
          ActiveDiscussionActionMenu(
            discussion,
            sendDiscussionAction,
            dismissActiveDiscussionMenu,
          )
        case None =>
          div()
      },
      if (hasAuthCookies) {
        div(
          ticketCenter(topicUpdates, discussionState, randomActionClient),
          topicUpdates.received --> Observer {
            (event: WebSocketMessageFromServer) =>
              connectionStatus.recordMessageReceived()

              event match
                case DiscussionActionConfirmedMessage(discussionEvent) =>
                  discussionEvent match
                    case DiscussionActionConfirmed.Rejected(
                          _: DiscussionAction.SwapTopics,
                        ) =>
                      connectionStatus.reportError(
                        "Swap failed: One or both topics were moved by another user. Please try again.",
                      )
                    case DiscussionActionConfirmed.Rejected(
                          _: DiscussionAction.SetRoomSlot,
                        ) =>
                      connectionStatus.reportError(
                        "Schedule change failed: topic or slot changed. Please try again.",
                      )
                    case DiscussionActionConfirmed.Rejected(
                          _: DiscussionAction.SetLockedTimeslot,
                        ) =>
                      connectionStatus.reportError(
                        "Lock update failed. Schedule may have changed.",
                      )
                    case DiscussionActionConfirmed.Unauthorized(_) =>
                      connectionStatus.reportError(
                        "Session expired. Re-authenticating...",
                      )
                    case DiscussionActionConfirmed.StateReplace(_, _) =>
                      connectionStatus.markStateSynchronized()
                    case DiscussionActionConfirmed.SwapTopics(
                          topic1,
                          newSlot1,
                          topic2,
                          newSlot2,
                        ) =>
                      SwapAnimationState.startSwapAnimation(
                        topic1,
                        newSlot1,
                        topic2,
                        newSlot2,
                      )
                    case DiscussionActionConfirmed.Vote(topicId, feedback) =>
                      val currentUser = name.now()
                      if feedback.voter == currentUser then
                        celebrateVote(topicId, feedback.position)
                        dismissSwipeHint()
                    case DiscussionActionConfirmed.SetRoomSlot(
                          topicId,
                          Some(newRoomSlot),
                        ) =>
                      discussionState
                        .now()
                        .data
                        .get(topicId)
                        .flatMap(_.roomSlot)
                        .foreach { oldSlot =>
                          SwapAnimationState.startMoveAnimation(
                            topicId,
                            oldSlot,
                            newRoomSlot,
                          )
                        }
                    case DiscussionActionConfirmed.SetRoomSlot(_, None) =>
                      ()
                    case DiscussionActionConfirmed.Delete(topicId) =>
                      SwapAnimationState.cleanupTopic(topicId)
                    case _ => ()

                  val restoreScroll = ScrollPreserver.captureScrollPosition()
                  discussionState
                    .update { existing =>
                      val state = existing(discussionEvent)
                      val (topicId, shouldClearActive) =
                        handleDiscussionActionConfirmed(discussionEvent)

                      if (shouldClearActive) {
                        activeDiscussion.set(None)
                      }

                      topicId.foreach { id =>
                        if (
                          activeDiscussion.now().map(_.id).contains(id)
                        ) {
                          activeDiscussion.set(state.data.get(id))
                        }
                      }

                      discussionEvent match
                        case DiscussionActionConfirmed.AddResult(discussion)
                            if discussion.roomSlot.isDefined =>
                          activeDiscussion.set(Some(discussion))
                        case _ => ()

                      state
                    }
                  restoreScroll()

                case LightningTalkActionConfirmedMessage(lightningEvent) =>
                  lightningEvent match
                    case LightningTalkActionConfirmed.Rejected(
                          _: LightningTalkAction.SetParticipation,
                        ) =>
                      connectionStatus.reportError(
                        "Lightning talk participation update failed.",
                      )
                    case LightningTalkActionConfirmed.Rejected(
                          LightningTalkAction.DrawForNextNight,
                        ) =>
                      connectionStatus.reportError(
                        "Draw not applied. Nights may already be full.",
                      )
                    case LightningTalkActionConfirmed.Rejected(_) =>
                      connectionStatus.reportError(
                        "Lightning talk update failed. Please refresh and try again.",
                      )
                    case LightningTalkActionConfirmed.Unauthorized(_) =>
                      connectionStatus.reportError(
                        "Session expired. Re-authenticating...",
                      )
                    case LightningTalkActionConfirmed.StateReplace(_) =>
                      connectionStatus.markStateSynchronized()
                    case _ => ()
                  lightningTalkState.update(existing => existing(lightningEvent))

                case HackathonProjectActionConfirmedMessage(hackathonEvent) =>
                  hackathonEvent match
                    case HackathonProjectActionConfirmed.Rejected(_) =>
                      connectionStatus.reportError(
                        "Hackathon project action failed. Please try again.",
                      )
                    case HackathonProjectActionConfirmed.Unauthorized(_) =>
                      connectionStatus.reportError(
                        "Session expired. Re-authenticating...",
                      )
                    case HackathonProjectActionConfirmed.StateReplace(_) =>
                      connectionStatus.markStateSynchronized()
                    case _ => ()
                  hackathonProjectState.update(existing => existing(hackathonEvent))

                case ActivityActionConfirmedMessage(activityEvent) =>
                  activityEvent match
                    case ActivityActionConfirmed.Rejected(_) =>
                      connectionStatus.reportError(
                        "Activity action failed. Please try again.",
                      )
                    case ActivityActionConfirmed.Unauthorized(_) =>
                      connectionStatus.reportError(
                        "Session expired. Re-authenticating...",
                      )
                    case ActivityActionConfirmed.StateReplace(_) =>
                      connectionStatus.markStateSynchronized()
                    case _ => ()
                  activityState.update(existing => existing(activityEvent))

                case _ =>
                  ()
          },
          errorBanner.component,
          // Admin mode toggle at the very top (only visible to admins)
          AdminModeToggle(isAdmin, adminModeEnabled),
          LoadingPreviewToggle(isAdmin, AppState.showLoadingPreview),
          NameBadge(name, connectionStatus.state, soundMuted),
          // Admin controls (only visible when admin AND admin mode enabled)
          AdminControls(
            isAdmin.combineWith(adminModeEnabled.signal).map { case (admin, enabled) => admin && enabled },
            sendDiscussionAction,
            connectionStatus,
            randomActionClient,
            Observer(_ => currentAppView.set(AppView.Replay)),
          ),
          ViewToggle(currentAppView, adminModeEnabled.signal),
          // Conditional view rendering based on current app view
          child <-- currentAppView.signal.map {
            case AppView.Admin =>
              ScheduleView(
                discussionState,
                activeDiscussion,
                sendDiscussionAction,
                name.signal,
                isAdmin,
                setActiveDiscussion,
                popoverState,
                swapMenuState,
                unscheduledMenuState,
                activeDiscussionMenuState,
              )
            case AppView.Topics =>
              liveTopicSubmissionAndVoting(updateTargetDiscussion)
            case AppView.LightningTalks =>
              ActivitiesView(
                lightningTalkState,
                activityState,
                name.signal,
                isAdmin.combineWith(adminModeEnabled.signal).map { case (admin, enabled) =>
                  admin && enabled
                },
                sendLightningAction,
                sendActivityAction,
                errorBanner.errorObserver,
                connectionStatus,
              )
            case AppView.Hackathon =>
              HackathonProjectsView(
                hackathonProjectState,
                name.signal,
                sendHackathonAction,
                errorBanner.errorObserver,
                connectionStatus,
              )
            case AppView.Schedule =>
              LinearScheduleView(
                discussionState.signal,
                lightningTalkState.signal,
                activityState.signal,
                sendDiscussionAction,
                sendActivityAction,
                name.signal,
                isAdmin,
                errorBanner.errorObserver,
                unscheduledMenuState,
              )
            case AppView.Replay =>
              ReplayView()
          },
        )
      } else {
        // Login screen - shown when user is not authenticated
        div(
          cls := "LoginScreen",
          img(
            cls := "LoginScreen-logo",
            src := "./wtf-web-nodate.jpg",
            alt := "Open Spaces",
          ),
          div(
            cls := "LoginScreen-content",
            h1(cls := "LoginScreen-title", "Welcome to Open Spaces"),
            p(
              cls := "LoginScreen-description",
              "The schedule is shaped by ",
              strong("your input"),
              ". Vote on topics you're interested in, propose your own discussions, and help build an unconference that reflects what the community wants to learn and share.",
            ),
            a(
              cls := "LoginScreen-button",
              href := "/auth",
              span(cls := "LoginScreen-buttonIcon"),
              span("Sign in with GitHub"),
            ),
          ),
        )
      },
    )

  render(container, app)
  }.provide(
    EndpointExecutor.make(serviceName = "open-spaces"),
    zio.http.Client.default,
    
  )

// NameBadge, BannerLogo, AdminModeToggle extracted to components/NameBadge.scala

// AdminControls extracted to components/AdminControls.scala
// TopicSubmission extracted to components/TopicSubmission.scala
// SwipeableCard extracted to components/SwipeableCard.scala

  private def handleDiscussionActionConfirmed(
    event: DiscussionActionConfirmed,
  ): (Option[TopicId], Boolean) =
    event match {
      case DiscussionActionConfirmed.Delete(topic) =>
        (Some(topic), true)
      case DiscussionActionConfirmed.Vote(topic, _) =>
        (Some(topic), false)
      case DiscussionActionConfirmed.ResetUser(_, _, _) =>
        (None, false)  // State is updated by the confirmed action itself
      case DiscussionActionConfirmed.Rename(topicId, _) =>
        (Some(topicId), false)
      case DiscussionActionConfirmed.SetRoomSlot(topicId, _) =>
        (Some(topicId), false)
      case DiscussionActionConfirmed.SetLockedTimeslot(topicId, _) =>
        (Some(topicId), false)
      case DiscussionActionConfirmed.SwapTopics(topic1, _, _, _) =>
        (Some(topic1), false)
      case DiscussionActionConfirmed.AddResult(_) =>
        (None, false)
      case DiscussionActionConfirmed.SlackThreadLinked(topicId, _) =>
        (Some(topicId), false)
      case DiscussionActionConfirmed.StateReplace(_, _) =>
        (None, false)
      case DiscussionActionConfirmed.Unauthorized(_) =>
        (None, false)
      case DiscussionActionConfirmed.Rejected(_) =>
        (None, false)
    }
  
  import io.laminext.websocket.*
  // WebSocket configuration constants
  private val MaxReconnectRetries = 10
  
  val topicUpdates
    : WebSocket[WebSocketMessageFromServer, WebSocketMessageFromClient] = {
    // If I don't confine the scope of it, it clashes with laminar's `span`. Weird.
    import scala.concurrent.duration._
      WebSocket
      .url("/discussions") // TODO Reference the Zio Endpoint to keep this in sync
      .text[WebSocketMessageFromServer, WebSocketMessageFromClient](
        _.toJson,
        _.fromJson[WebSocketMessageFromServer].left.map(Exception(_)),
      )
      .build(
        autoReconnect = true,
        reconnectDelay = 1.second,
        reconnectDelayOffline = 20.seconds,
        reconnectRetries = MaxReconnectRetries,
        bufferWhenDisconnected = true,  // Buffer messages during brief disconnects
        bufferSize = 10,                // Keep up to 10 pending messages
      )
  }
  
  // Connection status manager for monitoring WebSocket health, sync, and error handling
  // All reconnect/sync logic is consolidated here
  import scala.concurrent.ExecutionContext.Implicits.global
  val connectionStatus: ConnectionStatusCoordinator = new ConnectionStatusManager(
    topicUpdates,
    MaxReconnectRetries,
  )
  
  /** Sets up state synchronization on WebSocket (re)connection.
    *
    * Registers the sync operation with connectionStatus manager, which handles:
    * - Triggering sync on connect/reconnect
    * - Automatic retries with exponential backoff
    * - Coordination with visibility changes and health checks
    *
    * Also handles rejected actions by re-authenticating and retrying.
    */
  def ticketCenter(
    topicUpdates: WebSocket[WebSocketMessageFromServer, WebSocketMessageFromClient],
    discussionState: Var[DiscussionState],
    randomActionClient: RandomActionClient,
  ) =
    var authRefreshIntervalHandle: Option[Int] = None

    // Register the sync operation with the connection manager
    // This will be called on connect, reconnect, and visibility return
    connectionStatus.onSync {
      AuthService.fetchTicketAsync(randomActionClient).map { ticket =>
        println("Ticket received, sending to server for state sync")
        topicUpdates.sendOne(ticket)
      }
    }
  
    div(
      // Keep access token fresh while the app is open.
      onMountUnmountCallback(
        _ =>
          val intervalHandle = dom.window.setInterval(
            () =>
              AuthService.ensureFreshAccessToken(randomActionClient).foreach { refreshed =>
                if !refreshed then
                  dom.window.location.href = "/auth"
              },
            60 * 1000,
          )
          authRefreshIntervalHandle = Some(intervalHandle)
          AuthService.ensureFreshAccessToken(randomActionClient).foreach { refreshed =>
            if !refreshed then
              dom.window.location.href = "/auth"
          }
        ,
        _ =>
          authRefreshIntervalHandle.foreach(dom.window.clearInterval)
          authRefreshIntervalHandle = None,
      ),
      // Trigger sync when WebSocket connects
      topicUpdates.isConnected.changes.filter(identity) --> Observer { _ =>
        println("WebSocket connected - triggering sync via connectionStatus")
        connectionStatus.triggerSync()
      },
      // Also check on mount in case already connected
      onMountCallback { ctx =>
        val isConnected = topicUpdates.isConnected.observe(ctx.owner).now()
        if isConnected && connectionStatus.syncState.now() == SyncState.Idle then
          println("WebSocket already connected on mount - triggering sync")
          connectionStatus.triggerSync()
      },
      // Handle unauthorized actions by re-authenticating and retrying
      // Uses connectionStatus.withErrorHandling to catch and report errors
      topicUpdates.received.flatMapSwitch { event =>
        event match
          case DiscussionActionConfirmedMessage(
                DiscussionActionConfirmed.Unauthorized(discussionAction),
              ) =>
            EventStream.fromFuture(
              connectionStatus.withErrorHandling(
                AuthService.fetchTicketAsync(randomActionClient)
                  .map(ticket => (ticket, discussionAction)),
                "Re-authentication failed",
              )
            ).collect { case Some(result) => result }
          case LightningTalkActionConfirmedMessage(
                LightningTalkActionConfirmed.Unauthorized(lightningAction),
              ) =>
            EventStream.fromFuture(
              connectionStatus.withErrorHandling(
                AuthService.fetchTicketAsync(randomActionClient)
                  .map(ticket => (ticket, lightningAction)),
                "Re-authentication failed",
              )
            ).collect { case Some(result) => result }
          case HackathonProjectActionConfirmedMessage(
                HackathonProjectActionConfirmed.Unauthorized(hackathonAction),
              ) =>
            EventStream.fromFuture(
              connectionStatus.withErrorHandling(
                AuthService.fetchTicketAsync(randomActionClient)
                  .map(ticket => (ticket, hackathonAction)),
                "Re-authentication failed",
              )
            ).collect { case Some(result) => result }
          case ActivityActionConfirmedMessage(
                ActivityActionConfirmed.Unauthorized(activityAction),
              ) =>
            EventStream.fromFuture(
              connectionStatus.withErrorHandling(
                AuthService.fetchTicketAsync(randomActionClient)
                  .map(ticket => (ticket, activityAction)),
                "Re-authentication failed",
              )
            ).collect { case Some(result) => result }
          case _ =>
            EventStream.empty
      } --> { case (ticket, action) =>
        topicUpdates.sendOne(ticket)
        action match
          case action: DiscussionAction =>
            topicUpdates.sendOne(DiscussionActionMessage(action))
          case action: LightningTalkAction =>
            topicUpdates.sendOne(LightningTalkActionMessage(action))
          case action: HackathonProjectAction =>
            topicUpdates.sendOne(HackathonProjectActionMessage(action))
          case action: ActivityAction =>
            topicUpdates.sendOne(ActivityActionMessage(action))
      },
    )
}
