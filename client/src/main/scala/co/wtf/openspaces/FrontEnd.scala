package co.wtf.openspaces

import animus.*
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import org.scalajs.dom.window
import zio.json.*
import zio.*
import neotype.*

// Import extracted utilities
import co.wtf.openspaces.util.{SlotPositionTracker, SwapAnimationState, MenuPositioning, ScrollPreserver}
import co.wtf.openspaces.components.{ToastManager, AdminControls, TopicSubmission, SwipeableCard, ErrorBanner, VoteButtons, ViewToggle, InlineEditableTitle, NameBadge, AdminModeToggle, Menu, UnscheduledDiscussionsMenu, ActiveDiscussionActionMenu, TopicCard, DiscussionSubview, ScheduleSlotComponent, SlotSchedule, ScheduleView, SlotSchedules, LinearScheduleView, AppView, activeDiscussionLongPressBinder}
import co.wtf.openspaces.AppState.*
import co.wtf.openspaces.*
import co.wtf.openspaces.services.{AudioService, AuthService}
import zio.http.endpoint.{Endpoint, EndpointExecutor}

/** FrontEnd.scala - Main entry point and app composition
  * 
  * Utility objects have been extracted to:
  * - util/SlotPositionTracker.scala
  * - util/SwapAnimationState.scala  
  * - util/MenuPositioning.scala
  * - util/ScrollPreserver.scala
  * - components/Toast.scala
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
          "https://open-spaces-188fb0320ebe.herokuapp.com"
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
  val votedTopicOrder = AppState.votedTopicOrder
  val everVotedTopics = AppState.everVotedTopics
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

  /** Show a toast notification if the user cares about a topic that was scheduled/moved.
    * "Cares" means: user voted on it OR user is the facilitator.
    */
  def notifyScheduleChange(topicId: TopicId, newSlot: RoomSlot): Unit =
    val currentUser = name.now()
    discussionState.now().data.get(topicId).foreach { topic =>
      val userVoted = topic.interestedParties.exists(_.voter == currentUser)
      val userIsFacilitator = topic.facilitator == currentUser

      if userVoted || userIsFacilitator then
        val action = if userIsFacilitator then "Your topic" else "A topic you voted on"
        val message = s"""$action was scheduled: "${topic.topicName}" → ${newSlot.room.name} @ ${newSlot.timeSlot.s}"""
        ToastManager.show(message, "📍")
    }

  val errorBanner =
    ErrorBanner(connectionStatus)

  val submitNewTopic: Observer[DiscussionAction] = Observer {
    case discussion @ (add: DiscussionAction.Add) =>
      // Block submissions while not connected/synced
      if !connectionStatus.checkReady() then
        errorBanner.setError("Reconnecting... please wait and try again.")
      else if (add.facilitator.unwrap.trim.length < 2)
        errorBanner.setError("User name too short. Tell us who you are!")
      else
        errorBanner.clearError()
        topicUpdates.sendOne(discussion)
    case _ => ()
  }

  def liveTopicSubmissionAndVoting(
    updateTargetDiscussion: Observer[Discussion],
  ) =
    // Signal for counting unjudged topics
    val $unjudgedCount = Signal.combine(
      discussionState.signal,
      name.signal,
    ).map { case (state, currentUser) =>
      state.data.values.count { topic =>
        !topic.interestedParties.exists(_.voter == currentUser)
      }
    }

    div(
      TopicSubmission(submitNewTopic,
                      name.signal,
                      errorBanner.errorObserver,
      ),
      // Counter showing remaining topics to vote on
      div(
        cls := "UnjudgedCounter",
        child.text <-- $unjudgedCount.map { count =>
          if count == 0 then "✓ You've voted on all topics!"
          else if count == 1 then "1 topic left to vote on"
          else s"$count topics left to vote on"
        },
      ),
      {
        // Track the first unjudged topic ID for swipe hint
        val $firstUnjudgedId: Signal[Option[TopicId]] = Signal.combine(
          discussionState.signal,
          name.signal,
        ).map { case (state, currentUser) =>
          state.data.values.find { topic =>
            !topic.interestedParties.exists(_.voter == currentUser)
          }.map(_.id)
        }

        DiscussionSubview(
        Signal.combine(
          discussionState.signal,
          name.signal,
          votedTopicOrder.signal,
        ).map { case (state, currentUser, voteOrder) =>
          val allTopics = state.data.values.toList

          // Partition into judged (user has voted) and unjudged (user hasn't voted)
          val (judged, unjudged) = allTopics.partition { topic =>
            topic.interestedParties.exists(_.voter == currentUser)
          }

          // Take only the first unjudged topic (if any)
          val firstUnjudged = unjudged.headOption.toList

          // Sort judged topics by vote order (most recent first)
          // Topics not in voteOrder go to the end, sorted by ID for stability
          val orderIndex = voteOrder.zipWithIndex.toMap
          val sortedJudged = judged.sortBy { topic =>
            orderIndex.getOrElse(topic.id, Int.MaxValue)
          }

          // Combine: first unjudged + judged topics in vote order
          firstUnjudged ++ sortedJudged
        },
        None,
        name.signal,
        topicUpdates.sendOne,
        updateTargetDiscussion,
        connectionStatus,
        $firstUnjudgedId,
        showSwipeHint.signal,
      )
      },
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
      topicUpdates.sendOne(
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
      topicUpdates.closed --> Observer { (event: (dom.WebSocket, Boolean)) =>
        connectionStatus.closeObserver.onNext(event)
        connectionStatus.setConnected(false)
      },
      topicUpdates.connected --> Observer { (ws: dom.WebSocket) =>
        connectionStatus.connectedObserver.onNext(ws)
        connectionStatus.setConnected(true)
      },
      // Connection status banner (shows when disconnected/reconnecting/syncing)
      ConnectionStatusBanner.withSyncStatus(
        connectionStatus.state,
        connectionStatus.syncMessage,
        Observer(_ => connectionStatus.forceReconnect()),
      ),
      // Toast notifications for schedule changes
      ToastManager.component,
      // Popover component at top level
      // Swap action menu at top level
      child <-- swapMenuState.signal.map {
        case Some((selectedDiscussion, targetDiscussion)) =>
          Menu(
            selectedDiscussion,
            targetDiscussion,
            topicUpdates.sendOne,
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
              topicUpdates.sendOne,
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
            topicUpdates.sendOne,
            dismissActiveDiscussionMenu,
          )
        case None =>
          div()
      },
      if (hasAuthCookies) {
        div(
          ticketCenter(topicUpdates, discussionState, randomActionClient),
          topicUpdates.received --> Observer {
            (event: DiscussionActionConfirmed) =>
              // Record message receipt for health monitoring
              connectionStatus.recordMessageReceived()
              
              // Handle rejection feedback - use connectionStatus for error reporting
              event match
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
                case DiscussionActionConfirmed.Unauthorized(_) =>
                  connectionStatus.reportError(
                    "Session expired. Re-authenticating...",
                  )
                // Trigger swap animation before state update
                case DiscussionActionConfirmed.SwapTopics(
                      topic1,
                      newSlot1,
                      topic2,
                      newSlot2,
                    ) =>
                  // Start the swap animation
                  SwapAnimationState.startSwapAnimation(
                    topic1,
                    newSlot1,
                    topic2,
                    newSlot2,
                  )
                  // Show toast if user cares about either topic
                  notifyScheduleChange(topic1, newSlot1)
                  notifyScheduleChange(topic2, newSlot2)
                // Track the user's vote for topic ordering
                case DiscussionActionConfirmed.Vote(topicId, feedback) =>
                  val currentUser = name.now()
                  if feedback.voter == currentUser then
                    val isFirstVote = !everVotedTopics.now().contains(topicId)
                    everVotedTopics.update(_ + topicId)
                    // Move to front of vote order (prepend, removing any existing entry)
                    if isFirstVote then
                      votedTopicOrder.update(order => topicId :: order.filterNot(_ == topicId))
                    // Celebrate the vote! (sound + animation)
                    celebrateVote(topicId, feedback.position)
                    // Dismiss swipe hint on first vote
                    dismissSwipeHint()

                // Animate schedule updates for single-topic slot changes
                case DiscussionActionConfirmed.SetRoomSlot(
                      topicId,
                      Some(newRoomSlot),
                    ) =>
                  // Get the topic's current (old) position before state updates
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
                  // Show toast if user cares about this topic
                  notifyScheduleChange(topicId, newRoomSlot)
                case DiscussionActionConfirmed.SetRoomSlot(_, None) =>
                  ()
                // Clean up animation state when topics are deleted to prevent memory leaks
                case DiscussionActionConfirmed.Delete(topicId) =>
                  SwapAnimationState.cleanupTopic(topicId)
                // Track topics the user has voted on (for everVotedTopics set)
                // Also add newly created topics to vote order
                case DiscussionActionConfirmed.AddResult(discussion) =>
                  val currentUser = name.now()
                  if discussion.interestedParties.exists(_.voter == currentUser) then
                    everVotedTopics.update(_ + discussion.id)
                  // If user just created this topic, add to front of vote order
                  // (creating a topic = auto-voting Interested on it)
                  if discussion.facilitator == currentUser then
                    votedTopicOrder.update(order => discussion.id :: order.filterNot(_ == discussion.id))
                case _ => ()

              // Capture scroll position before state update
              val restoreScroll = ScrollPreserver.captureScrollPosition()
              
              discussionState
                .update { existing =>
                  val state = existing(event)
                  val (topicId, shouldClearActive) =
                    handleDiscussionActionConfirmed(event)

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

                  // Auto-select newly created topics that are scheduled in a room slot
                  event match
                    case DiscussionActionConfirmed.AddResult(discussion)
                        if discussion.roomSlot.isDefined =>
                      activeDiscussion.set(Some(discussion))
                    case _ => ()

                  state
                }
              
              // Restore scroll position after DOM updates
              restoreScroll()
          },
          errorBanner.component,
          // Admin mode toggle at the very top (only visible to admins)
          AdminModeToggle(isAdmin, adminModeEnabled),
          NameBadge(name, connectionStatus.state, soundMuted),
          // Admin controls (only visible when admin AND admin mode enabled)
          AdminControls(
            isAdmin.combineWith(adminModeEnabled.signal).map { case (admin, enabled) => admin && enabled },
            topicUpdates.sendOne,
            connectionStatus,
            randomActionClient
          ),
          ViewToggle(currentAppView, adminModeEnabled.signal),
          // Conditional view rendering based on current app view
          child <-- currentAppView.signal.map {
            case AppView.Admin =>
              ScheduleView(
                discussionState,
                activeDiscussion,
                topicUpdates.sendOne,
                name.signal,
                setActiveDiscussion,
                popoverState,
                swapMenuState,
                unscheduledMenuState,
                activeDiscussionMenuState,
              )
            case AppView.Topics =>
              liveTopicSubmissionAndVoting(updateTargetDiscussion)
            case AppView.Schedule =>
              LinearScheduleView(
                discussionState.signal,
                topicUpdates.sendOne,
                name.signal,
                unscheduledMenuState,
              )
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
      case DiscussionActionConfirmed.SwapTopics(topic1, _, _, _) =>
        (Some(topic1), false)
      case DiscussionActionConfirmed.AddResult(_) =>
        (None, false)
      case DiscussionActionConfirmed.SlackThreadLinked(topicId, _) =>
        (Some(topicId), false)
      case DiscussionActionConfirmed.StateReplace(_) =>
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
    : WebSocket[DiscussionActionConfirmed, WebSocketMessage] = {
    // If I don't confine the scope of it, it clashes with laminar's `span`. Weird.
    import scala.concurrent.duration._
    WebSocket
      .url("/discussions")
      .text[DiscussionActionConfirmed, WebSocketMessage](
        _.toJson,
        _.fromJson[DiscussionActionConfirmed].left.map(Exception(_)),
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
  val connectionStatus: ConnectionStatusManager[DiscussionActionConfirmed, WebSocketMessage] = new ConnectionStatusManager(
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
    topicUpdates: WebSocket[DiscussionActionConfirmed, WebSocketMessage],
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
          case DiscussionActionConfirmed.Unauthorized(discussionAction) =>
            EventStream.fromFuture(
              connectionStatus.withErrorHandling(
                AuthService.fetchTicketAsync(randomActionClient)
                  .map(ticket => (ticket, discussionAction)),
                "Re-authentication failed",
              )
            ).collect { case Some(result) => result }
          case _ =>
            EventStream.empty
      } --> { case (ticket, discussionAction) =>
        topicUpdates.sendOne(ticket)
        topicUpdates.sendOne(discussionAction)
      },
    )
}
