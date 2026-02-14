package co.wtf.openspaces

import animus.*
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import org.scalajs.dom.window
import zio.json.*
import neotype.*

val localStorage = window.localStorage

import org.scalajs.dom
import scala.scalajs.js.URIUtils

/** Tracks the DOM positions of each schedule slot so animations can
  * start from the actual on-screen location rather than an estimated
  * grid slot.
  */
object SlotPositionTracker:
  private val slotElements: Var[Map[RoomSlot, dom.Element]] = Var(
    Map.empty,
  )
  // Keep last known centers so animations still have a reasonable start
  // point even if a slot temporarily unmounts (e.g., virtualization or
  // view changes).
  private val lastKnownCenters: Var[Map[RoomSlot, (Double, Double)]] =
    Var(
      Map.empty,
    )
  
  // Timestamp of last bulk position update to throttle reflows
  private var lastBulkUpdateMs: Double = 0
  private val BulkUpdateThrottleMs: Double = 100 // Only refresh positions every 100ms max

  /** Register a slot element so we can compute its live center
    * position.
    *
    * @return
    *   a cleanup function to unregister on unmount
    */
  def register(
    slot: RoomSlot,
    element: dom.Element,
  ): () => Unit =
    slotElements.update(_ + (slot -> element))
    // Capture an initial center right away (only on registration, not every access)
    updateCenterNoThrottle(slot, element)
    () =>
      slotElements.update(_ - slot)
      () // keep lastKnownCenters for fallback

  /** Current center point for a slot, if its element is mounted.
    * Uses cached positions to avoid layout thrashing.
    */
  def center(
    slot: RoomSlot,
  ): Option[(Double, Double)] =
    // First try cached position (no reflow)
    lastKnownCenters.now().get(slot).orElse {
      // Only compute fresh if not cached
      slotElements
        .now()
        .get(slot)
        .flatMap(el => updateCenterNoThrottle(slot, el))
    }

  /** Force refresh all slot positions. Call this sparingly (e.g., on resize). */
  def refreshAllPositions(): Unit =
    val now = System.currentTimeMillis().toDouble
    if now - lastBulkUpdateMs > BulkUpdateThrottleMs then
      lastBulkUpdateMs = now
      slotElements.now().foreach { case (slot, element) =>
        updateCenterNoThrottle(slot, element)
      }

  private def updateCenterNoThrottle(
    slot: RoomSlot,
    element: dom.Element,
  ): Option[(Double, Double)] =
    val rect = element.getBoundingClientRect()
    val center =
      (rect.left + rect.width / 2, rect.top + rect.height / 2)
    lastKnownCenters.update(_ + (slot -> center))
    Some(center)

  /** Pixel delta from one slot center to another, if both are known.
    */
  def offsetBetween(
    fromSlot: RoomSlot,
    toSlot: RoomSlot,
  ): Option[(Double, Double)] =
    for
      from <- center(fromSlot)
      to   <- center(toSlot)
    yield (from._1 - to._1, from._2 - to._2)

/** Tracks active swap animations for smooth position transitions.
  *
  * Animation flow:
  *   1. When a swap/move is confirmed, we set initial offsets (from
  *      old position to new) 2. After a brief delay (one animation
  *      frame), we clear the offsets to (0, 0) 3. The spring
  *      animation interpolates from the offset to (0, 0), creating
  *      the sliding effect 4. Cleanup happens after the spring
  *      settles, but only if no newer animation has started for that
  *      topic
  */
object SwapAnimationState:
  // Stores the INITIAL offsets for each topic (where they should appear to come FROM)
  val initialOffsets: Var[Map[TopicId, (Double, Double)]] = Var(
    Map.empty,
  )

  // Stores whether each topic's animation should be "animating to zero"
  val animatingToZero: Var[Set[TopicId]] = Var(Set.empty)

  // Tracks animation "generation" per topic to prevent stale cleanup
  // Each new animation increments the generation, and cleanup only runs
  // if the generation hasn't changed (i.e., no newer animation started)
  private val animationGeneration: Var[Map[TopicId, Int]] = Var(
    Map.empty,
  )

  /** Immediately clean up all animation state for a topic.
    * Call this when a topic is deleted to prevent memory leaks.
    */
  def cleanupTopic(topicId: TopicId): Unit =
    initialOffsets.update(_ - topicId)
    animatingToZero.update(_ - topicId)
    animationGeneration.update(_ - topicId)

  /** Spring animation typically needs ~600-800ms to settle */
  val cleanupDelayMs: Int = 1000

  // Grid cell dimensions for offset calculations
  private val cellWidth = 60.0 // Approximate cell width in pixels
  // Use a smaller vertical offset since row calculations can over-estimate
  // the visual distance for adjacent time slots
  private val cellHeight = 40.0

  /** Calculates the pixel offset between two room slots */
  private def calculateOffset(
    fromSlot: RoomSlot,
    toSlot: RoomSlot,
  ): (Double, Double) =
    SlotPositionTracker.offsetBetween(fromSlot, toSlot).getOrElse {
      val colDiff = fromSlot.room.id - toSlot.room.id
      val rowDiff = timeSlotToRow(fromSlot.timeSlot) - timeSlotToRow(
        toSlot.timeSlot,
      )
      (colDiff * cellWidth, rowDiff * cellHeight)
    }

  /** Increments and returns the new animation generation for a topic
    */
  private def nextGeneration(
    topicId: TopicId,
  ): Int =
    val newGen = animationGeneration.now().getOrElse(topicId, 0) + 1
    animationGeneration.update(_ + (topicId -> newGen))
    newGen

  /** Cleans up animation state for a topic, but only if the
    * generation matches
    */
  private def cleanupIfSameGeneration(
    topicId: TopicId,
    expectedGen: Int,
  ): Unit =
    if (
      animationGeneration.now().get(topicId).contains(expectedGen)
    ) {
      initialOffsets.update(_ - topicId)
      animatingToZero.update(_ - topicId)
      animationGeneration.update(_ - topicId)
    }
    // If generation doesn't match, a newer animation is in progress - don't cleanup

  /** Starts an animation for a single topic being moved from one slot
    * to another
    */
  def startMoveAnimation(
    topicId: TopicId,
    fromSlot: RoomSlot,
    toSlot: RoomSlot,
  ): Unit =
    // Calculate offset: topic is now at toSlot, but should appear to come FROM fromSlot
    val offset = calculateOffset(fromSlot, toSlot)

    // Get a new generation number for this animation
    val gen = nextGeneration(topicId)

    // Step 1: Set initial offset (topic appears at its OLD position)
    initialOffsets.update(_ + (topicId -> offset))

    // Step 2: After a brief delay, trigger animation to zero
    window.setTimeout(
      () => animatingToZero.update(_ + topicId),
      16, // ~1 animation frame
    )

    // Step 3: Clean up after animation completes (only if no newer animation started)
    window.setTimeout(
      () => cleanupIfSameGeneration(topicId, gen),
      cleanupDelayMs,
    )

  /** Starts a swap animation between two topics based on their
    * RoomSlots
    */
  def startSwapAnimation(
    topic1: TopicId,
    slot1: RoomSlot,
    topic2: TopicId,
    slot2: RoomSlot,
  ): Unit =
    // After swap, topic1 is now at slot1, but WAS at slot2 before
    // So topic1 needs offset from slot2 to slot1
    val topic1Offset = calculateOffset(slot2, slot1)
    val topic2Offset = calculateOffset(slot1, slot2)

    // Get new generation numbers for both topics
    val gen1 = nextGeneration(topic1)
    val gen2 = nextGeneration(topic2)

    // Step 1: Set initial offsets (topics appear at their OLD positions)
    initialOffsets.update(
      _ + (topic1 -> topic1Offset) + (topic2 -> topic2Offset),
    )

    // Step 2: After a brief delay, trigger animation to zero
    window.setTimeout(
      () => animatingToZero.update(_ + topic1 + topic2),
      16, // ~1 animation frame
    )

    // Step 3: Clean up after animation completes (only if no newer animations started)
    window.setTimeout(
      () => {
        cleanupIfSameGeneration(topic1, gen1)
        cleanupIfSameGeneration(topic2, gen2)
      },
      cleanupDelayMs,
    )

  /** Gets the animated offset signal for a topic. Returns the initial
    * offset if not yet animating, or (0,0) if animating to final
    * position. The spring will interpolate between these values.
    */
  def getOffsetSignal(
    topicId: TopicId,
  ): Signal[(Double, Double)] =
    initialOffsets.signal.combineWith(animatingToZero.signal).map {
      case (offsets, animating) =>
        if (animating.contains(topicId)) {
          // Target position: animate TO (0, 0)
          (0.0, 0.0)
        }
        else {
          // Initial position: offset from old position
          offsets.getOrElse(topicId, (0.0, 0.0))
        }
    }

  /** Helper to convert TimeSlot to a row index for offset calculation
    */
  private def timeSlotToRow(
    timeSlot: TimeSlot,
  ): Int =
    val startTime = timeSlot.startTime
    val dayOffset = startTime.getDayOfYear * 100
    val timeOffset = startTime.getHour * 60 + startTime.getMinute
    (dayOffset + timeOffset) / 50

/** Centralized menu positioning - uses viewport dimensions only */
object MenuPositioning:
  private val menuMargin = 10.0
  private val standardMenuMaxWidth = 350.0

  /** Standard positioning for dropdown menus - centered horizontally,
    * near top of screen
    */
  def standardMenuPosition(): (Double, Double) =
    val viewportHeight = dom.window.innerHeight
    val viewportWidth = dom.window.innerWidth

    val menuWidth =
      Math.min(standardMenuMaxWidth, viewportWidth - 2 * menuMargin)

    // Center the menu horizontally
    val x = (viewportWidth - menuWidth) / 2

    // Position near top of screen to ensure visibility
    val y = Math.max(20.0, viewportHeight * 0.05)

    (x, y)

/** Preserves scroll position of the TimeSlots container across state updates.
  * Captures scroll position before updates and restores it after DOM settles.
  */
object ScrollPreserver:
  private val timeSlotsId = "time-slots-container"
  
  def getTimeSlotsElement: Option[dom.html.Element] =
    Option(dom.document.getElementById(timeSlotsId))
      .map(_.asInstanceOf[dom.html.Element])
  
  /** Captures current scroll position and returns a restore function */
  def captureScrollPosition(): () => Unit =
    getTimeSlotsElement match
      case Some(element) =>
        val scrollTop = element.scrollTop
        () => {
          // Use setTimeout to restore after DOM updates
          window.setTimeout(
            () => getTimeSlotsElement.foreach(_.scrollTop = scrollTop),
            0
          )
        }
      case None =>
        () => () // No-op if element not found

  /** The id attribute to add to the TimeSlots container */
  val timeSlotsIdAttr: Modifier[HtmlElement] = idAttr := timeSlotsId

/** Toast notification manager for user alerts (e.g., "Your topic was scheduled") */
object ToastManager:
  case class Toast(id: Int, message: String, icon: String = "📍")
  
  private var nextId = 0
  val toasts: Var[List[Toast]] = Var(Nil)
  
  private val displayDurationMs = 5000
  private val maxToasts = 3
  
  def show(message: String, icon: String = "📍"): Unit =
    val id = nextId
    nextId += 1
    val toast = Toast(id, message, icon)
    
    // Add toast, keeping only the most recent maxToasts
    toasts.update(ts => (toast :: ts).take(maxToasts))
    
    // Auto-dismiss after duration
    dom.window.setTimeout(
      () => dismiss(id),
      displayDurationMs
    )
  
  def dismiss(id: Int): Unit =
    toasts.update(_.filterNot(_.id == id))
  
  /** The toast container component - render this at the top level */
  val component: HtmlElement =
    div(
      cls := "ToastContainer",
      children <-- toasts.signal.map { ts =>
        ts.map { toast =>
          div(
            cls := "Toast",
            span(cls := "Toast-icon", toast.icon),
            span(cls := "Toast-message", toast.message),
            button(
              cls := "Toast-dismiss",
              onClick --> Observer(_ => dismiss(toast.id)),
              "×"
            )
          )
        }
      }
    )

object FrontEnd extends App:
  ServiceWorkerClient.registerServiceWorker()
  lazy val container = dom.document.getElementById("app")

  val discussionState: Var[DiscussionState] =
    Var(
      DiscussionState(DiscussionState.timeSlotExamples, Map.empty),
    )

  // Tracks the order of topics the user has voted on / created.
  // Most recent first. Used to maintain stable ordering of judged topics.
  val votedTopicOrder: Var[List[TopicId]] =
    Var(Nil)

  // Tracks all topics the user has ever voted on (monotonically grows).
  // Used to distinguish first votes from vote changes.
  val everVotedTopics: Var[Set[TopicId]] =
    Var(Set.empty)

  // ============================================
  // Vote Celebration (Sound + Visual Effects)
  // ============================================
  
  // Sound muted state - persisted to localStorage
  val soundMuted: Var[Boolean] = Var {
    Option(localStorage.getItem("soundMuted")).contains("true")
  }
  
  // Persist sound muted state to localStorage
  soundMuted.signal.foreach { muted =>
    localStorage.setItem("soundMuted", muted.toString)
  }(unsafeWindowOwner)
  
  // Topics currently showing celebration animation (cleared after animation ends)
  val celebratingTopics: Var[Map[TopicId, VotePosition]] = Var(Map.empty)
  
  // Shared AudioContext - created lazily on first user interaction
  private var sharedAudioContext: Option[dom.AudioContext] = None
  
  /** Get or create the shared AudioContext, resuming if suspended */
  private def getAudioContext(): Option[dom.AudioContext] =
    sharedAudioContext match
      case Some(ctx) =>
        // Resume if suspended (browser policy)
        if ctx.state == "suspended" then
          ctx.resume()
        Some(ctx)
      case None =>
        try
          val ctx = new dom.AudioContext()
          sharedAudioContext = Some(ctx)
          Some(ctx)
        catch
          case _: Throwable => None
  
  /** Initialize audio on first user gesture (call from any click/touch handler) */
  def initAudioOnGesture(): Unit =
    if sharedAudioContext.isEmpty then
      getAudioContext()
    else
      sharedAudioContext.foreach { ctx =>
        if ctx.state == "suspended" then ctx.resume()
      }
  
  /** Play a satisfying pop/click sound using Web Audio API */
  def playVoteSound(position: VotePosition): Unit =
    if !soundMuted.now() then
      getAudioContext().foreach { audioContext =>
        try
          val oscillator = audioContext.createOscillator()
          val gainNode = audioContext.createGain()
          
          // Different sounds for interested vs not interested
          val (startFreq, endFreq) = position match
            case VotePosition.Interested => (600.0, 800.0)  // Rising pop - happy
            case VotePosition.NotInterested => (400.0, 300.0) // Falling thud - muted
          
          oscillator.`type` = "sine"
          oscillator.frequency.setValueAtTime(startFreq, audioContext.currentTime)
          oscillator.frequency.exponentialRampToValueAtTime(endFreq, audioContext.currentTime + 0.08)
          
          gainNode.gain.setValueAtTime(0.15, audioContext.currentTime)
          gainNode.gain.exponentialRampToValueAtTime(0.01, audioContext.currentTime + 0.12)
          
          oscillator.connect(gainNode)
          gainNode.connect(audioContext.destination)
          
          oscillator.start(audioContext.currentTime)
          oscillator.stop(audioContext.currentTime + 0.12)
        catch
          case _: Throwable => () // Silently fail if audio not available
      }
  
  /** Trigger celebration for a topic (animation + sound) */
  def celebrateVote(topicId: TopicId, position: VotePosition): Unit =
    // Add to celebrating set
    celebratingTopics.update(_ + (topicId -> position))
    
    // Play sound
    playVoteSound(position)
    
    // Remove from celebrating after animation completes (400ms)
    dom.window.setTimeout(() => {
      celebratingTopics.update(_ - topicId)
    }, 450)

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
        val message = s"$action was scheduled: \"${topic.topicName}\" → ${newSlot.room.name} @ ${newSlot.timeSlot.s}"
        ToastManager.show(message, "📍")
    }

  // ============================================
  // Swipe Hint (one-time onboarding)
  // ============================================
  
  // Whether user has seen the swipe hint (persisted)
  val hasSeenSwipeHint: Var[Boolean] = Var {
    Option(localStorage.getItem("hasSeenSwipeHint")).contains("true")
  }
  
  // Whether to currently show the swipe hint UI
  val showSwipeHint: Var[Boolean] = Var {
    !Option(localStorage.getItem("hasSeenSwipeHint")).contains("true")
  }
  
  /** Dismiss the swipe hint and remember it */
  def dismissSwipeHint(): Unit =
    showSwipeHint.set(false)
    hasSeenSwipeHint.set(true)
    localStorage.setItem("hasSeenSwipeHint", "true")

  val errorBanner =
    ErrorBanner()

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

  val name = getGitHubUsername()
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
        $firstUnjudgedId,
        showSwipeHint.signal,
      )
      },
    )

  val activeDiscussion: Var[Option[Discussion]] =
    Var(None)

  // Popover state
  val popoverState: Var[Option[Discussion]] =
    Var(None)

  // Swap action menu state: (selected Discussion, target Discussion)
  val swapMenuState: Var[Option[(Discussion, Discussion)]] =
    Var(None)

  // Unscheduled discussions menu state
  val unscheduledMenuState: Var[Option[RoomSlot]] =
    Var(None)

  // Active discussion actions menu state
  val activeDiscussionMenuState: Var[Option[Discussion]] =
    Var(None)

  // Current app view (Schedule or Topics)
  val currentAppView: Var[AppView] =
    Var(AppView.Topics)
  
  // Admin check - only admins can see admin controls
  val isAdmin: Signal[Boolean] = name.signal.map { person =>
    List("swoogles", "emma").exists(admin =>
      person.unwrap.toLowerCase().contains(admin)
    )
  }

  // Admin mode toggle - when false, admins see the normal user view
  val adminModeEnabled: Var[Boolean] = Var(
    localStorage.getItem("adminModeEnabled") == "true"
  )
  
  // Persist admin mode preference
  adminModeEnabled.signal.foreach { enabled =>
    localStorage.setItem("adminModeEnabled", enabled.toString)
  }(unsafeWindowOwner)

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
      discussion.roomSlot match
        case Some(value) =>
          topicUpdates.sendOne(
            DiscussionAction.UpdateRoomSlot(discussion.id, value),
          )
        case None =>
          topicUpdates.sendOne(
            DiscussionAction.Unschedule(discussion.id),
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
    getCookie("access_token").isDefined ||
      getCookie("access_token_expires_at").isDefined

  val app =
    div(
      cls := "PageContainer",
      // Initialize audio on first user interaction (required by browser autoplay policy)
      onClick --> Observer(_ => initAudioOnGesture()),
      onTouchStart --> Observer(_ => initAudioOnGesture()),
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
          ticketCenter(topicUpdates, discussionState),
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
                      _: DiscussionAction.MoveTopic,
                    ) =>
                  connectionStatus.reportError(
                    "Move failed: That slot was just filled by another user. Please try again.",
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
                // Trigger move animation for single topic moves
                case DiscussionActionConfirmed.MoveTopic(
                      topicId,
                      targetRoomSlot,
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
                        targetRoomSlot,
                      )
                    }
                  // Show toast if user cares about this topic (voted or facilitating)
                  notifyScheduleChange(topicId, targetRoomSlot)
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

                // Also animate UpdateRoomSlot (similar to MoveTopic)
                case DiscussionActionConfirmed.UpdateRoomSlot(
                      topicId,
                      newRoomSlot,
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

def getCookie(
  name: String,
): Option[String] = {
  val cookieString = dom.document.cookie
  val cookies = cookieString.split(";")

  cookies.find(_.trim.startsWith(s"$name=")) match {
    case Some(cookie) =>
      val encodedValue = cookie.trim.substring(name.length + 1)
      Some(URIUtils.decodeURIComponent(encodedValue))
    case None => None
  }
}

/** Check if the access token is expired or about to expire (within 5 minutes) */
def isAccessTokenExpired: Boolean = {
  val accessToken = getCookie("access_token")
  if (accessToken.isEmpty) {
    true
  } else {
    getCookie("access_token_expires_at") match {
      case Some(expiresAt) =>
        val expiryTime = expiresAt.toLongOption.getOrElse(0L)
        val now = (System.currentTimeMillis() / 1000)
        val bufferSeconds = 300 // 5 minute buffer
        now >= (expiryTime - bufferSeconds)
      case None =>
        // No expiry cookie means old session - treat as expired to trigger refresh
        true
    }
  }
}

/** Refresh the access token using the refresh token */
def refreshAccessToken(): scala.concurrent.Future[Boolean] = {
  import scala.scalajs.js
  import scala.concurrent.ExecutionContext.Implicits.global
  
  val fetchPromise = dom.fetch("/refresh", new dom.RequestInit {
    method = dom.HttpMethod.GET
  })
  
  fetchPromise.toFuture.map { response =>
    if (response.ok) {
      // Cookies are automatically updated by the response
      true
    } else {
      // Refresh failed - will redirect to login
      false
    }
  }.recover { case _ => false }
}

/** Gets the GitHub username from the cookie set during OAuth login. The name
  * is immutable and comes directly from GitHub.
  */
private def getGitHubUsername(): Var[Person] =
  val username = getCookie("github_username")
    .map(Person(_))
    .getOrElse(Person(""))
  Var(username)

private def BannerLogo() =
  div(width := "100%",
      img(cls := "LogoImg",
          src := "./wtf-web-nodate.jpg",
          role := "img",
      ),
  )

private def NameBadge(
  name: Var[Person],
  connectionState: Signal[ConnectionState],
  soundMuted: Var[Boolean],
) =
  div(
    cls := "Banner",
    img(cls := "LogoImg",
        src := "./wtf-web-nodate.jpg",
        role := "img",
    ),
    div(
      cls := "UserProfileSection",
      // Sound toggle button
      button(
        cls <-- soundMuted.signal.map { muted =>
          if muted then "sound-toggle sound-toggle--muted"
          else "sound-toggle"
        },
        title <-- soundMuted.signal.map { muted =>
          if muted then "Sound off - click to enable"
          else "Sound on - click to mute"
        },
        aria.label <-- soundMuted.signal.map { muted =>
          if muted then "Enable sound effects"
          else "Mute sound effects"
        },
        child.text <-- soundMuted.signal.map { muted =>
          if muted then "🔇" else "🔊"
        },
        onClick --> Observer { _ =>
          soundMuted.update(!_)
        },
      ),
      // Connection status indicator dot
      ConnectionStatusIndicator.dot(connectionState),
      // Profile icon - click to logout
      span(
        cls := "ProfileIconButton",
        title := "Click to logout",
        cursor := "pointer",
        child <-- name.signal.map { person =>
          GitHubAvatar(person, "github-avatar")
        },
        onClick --> Observer { _ =>
          if (dom.window.confirm("Log out?")) {
            deleteCookie("access_token")
            deleteCookie("access_token_expires_at")
            deleteCookie("github_username")
            dom.window.location.reload()
          }
        },
      ),
    ),
  )

/** Admin mode toggle - shows at very top, only for admins */
private def AdminModeToggle(
  $isAdmin: Signal[Boolean],
  adminModeEnabled: Var[Boolean],
) =
  div(
    cls := "AdminModeToggle",
    // Only show for admins
    display <-- $isAdmin.map(if _ then "flex" else "none"),
    label(
      cls := "AdminModeToggle-label",
      input(
        typ := "checkbox",
        checked <-- adminModeEnabled.signal,
        onChange.mapToChecked --> adminModeEnabled,
      ),
      span("Admin Mode"),
    ),
  )

/** Admin controls - chaos buttons, delete all, and reset user */
private def AdminControls(
  $showAdminControls: Signal[Boolean],
  topicUpdates: DiscussionAction => Unit,
) =
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

  case class ScheduleResult(scheduled: Int, moved: Int, unscheduled: Int) derives JsonCodec
  case class VersionInfo(version: String) derives JsonCodec

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

case class RandomActionStatus(active: Boolean) derives JsonCodec

private def TopicSubmission(
  submitEffect: Observer[DiscussionAction],
  name: StrictSignal[Person],
  setErrorMsg: Observer[Option[String]],
) =
  val textVar = Var("")
  val isFocused = Var(false)
  
  div(
    cls := "TopicSubmission",
    cls <-- isFocused.signal.map(f => if f then "TopicSubmission--focused" else ""),
    div(
      cls := "TopicSubmission-inputWrapper",
      textArea(
        cls := "TopicSubmission-textArea",
        placeholder := "What topic would you like to discuss?",
        value <-- textVar,
        onInput.mapToValue --> textVar,
        onFocus --> Observer(_ => isFocused.set(true)),
        onBlur --> Observer(_ => isFocused.set(false)),
      ),
    ),
    button(
      cls := "TopicSubmission-button",
      onClick
        .mapTo(textVar.now())
        .map(s =>
          val res = Topic.make(s)
          res match
            case Left(value) =>
              setErrorMsg.onNext(Some(value))
              None
            case Right(value) =>
              Some(value),
        )
        .filter(_.isDefined)
        .map(_.get)
        .map(topicTitle =>
          DiscussionAction.Add(
            topicTitle,
            name.now(),
          ),
        )
        .tapEach { case _ =>
          textVar.set("")
        } --> submitEffect,
      span("Submit Topic"),
    ),
  )

/** Swipe state for tracking drag gestures */
case class SwipeState(
  isDragging: Boolean = false,
  startX: Double = 0,
  currentX: Double = 0,
  cardWidth: Double = 300,
):
  /** Horizontal offset from start position */
  def offsetX: Double = if isDragging then currentX - startX else 0
  
  /** Progress toward threshold (-1 to 1, negative = left, positive = right) */
  def progress: Double = 
    val threshold = cardWidth * 0.35
    (offsetX / threshold).max(-1.5).min(1.5)
  
  /** Whether we've crossed the commit threshold */
  def isPastThreshold: Boolean = math.abs(progress) >= 1.0
  
  /** The vote direction if past threshold */
  def voteDirection: Option[VotePosition] =
    if !isPastThreshold then None
    else if offsetX > 0 then Some(VotePosition.Interested)
    else Some(VotePosition.NotInterested)

/** Swipeable wrapper for topic cards that enables swipe-to-vote.
  * 
  * - Swipe right → Interested (green)
  * - Swipe left → Not Interested (gray)
  * - Visual feedback: color gradient + icon reveal during drag
  * - Threshold at ~35% of card width
  * - Rubber-bands back if released before threshold
  */
def SwipeableCard(
  topic: Discussion,
  name: StrictSignal[Person],
  topicUpdates: DiscussionAction => Unit,
  cardContent: HtmlElement,
) =
  val swipeState: Var[SwipeState] = Var(SwipeState())
  
  // Animation state for rubber-band effect
  val isAnimating: Var[Boolean] = Var(false)
  
  def handleDragStart(clientX: Double, element: dom.Element): Unit =
    if !isAnimating.now() then
      val rect = element.getBoundingClientRect()
      swipeState.set(SwipeState(
        isDragging = true,
        startX = clientX,
        currentX = clientX,
        cardWidth = rect.width,
      ))
  
  def handleDragMove(clientX: Double): Unit =
    if swipeState.now().isDragging then
      swipeState.update(_.copy(currentX = clientX))
  
  def handleDragEnd(): Unit =
    val state = swipeState.now()
    if state.isDragging then
      state.voteDirection match
        case Some(position) =>
          // Only commit the vote if connection is ready
          // This prevents actions during sync/reconnect which would be rejected
          if connectionStatus.checkReady() then
            val voter = name.now()
            topicUpdates(DiscussionAction.Vote(topic.id, Feedback(voter, position)))
          else
            println("Connection not ready, ignoring vote action")
        case None =>
          // Rubber-band back
          ()
      
      // Reset with animation
      isAnimating.set(true)
      swipeState.set(SwipeState())
      // Clear animation flag after transition
      val _ = window.setTimeout(() => isAnimating.set(false), 300)
  
  // Calculate dynamic styles based on swipe state
  val $transform: Signal[String] = swipeState.signal.combineWith(isAnimating.signal).map {
    case (state, animating) =>
      if animating then "translateX(0px)"
      else if state.isDragging then s"translateX(${state.offsetX}px)"
      else "translateX(0px)"
  }
  
  val $revealOpacity: Signal[Double] = swipeState.signal.map { state =>
    math.abs(state.progress).min(1.0)
  }
  
  val $revealDirection: Signal[String] = swipeState.signal.map { state =>
    if state.offsetX >= 0 then "right" else "left"
  }
  
  div(
    cls := "SwipeableCardContainer",
    // Left reveal (not interested - gray)
    div(
      cls := "SwipeReveal SwipeReveal--left",
      opacity <-- swipeState.signal.map { state =>
        if state.offsetX < 0 then math.abs(state.progress).min(1.0) else 0
      },
      // Icon scales up as you approach threshold
      div(
        cls := "SwipeRevealIcon",
        transform <-- swipeState.signal.map { state =>
          val scale = if state.offsetX < 0 then math.abs(state.progress).min(1.0) else 0
          s"scale($scale)"
        },
        "✗",
      ),
    ),
    // Right reveal (interested - green)
    div(
      cls := "SwipeReveal SwipeReveal--right",
      opacity <-- swipeState.signal.map { state =>
        if state.offsetX > 0 then state.progress.min(1.0) else 0
      },
      div(
        cls := "SwipeRevealIcon",
        transform <-- swipeState.signal.map { state =>
          val scale = if state.offsetX > 0 then state.progress.min(1.0) else 0
          s"scale($scale)"
        },
        "♥",
      ),
    ),
    // The actual card content (slides)
    div(
      cls := "SwipeableCardContent",
      cls <-- isAnimating.signal.map(if _ then "SwipeableCardContent--animating" else ""),
      transform <-- $transform,
      // Touch events
      onTouchStart --> { (e: dom.TouchEvent) =>
        // Use elementFromPoint to get the ACTUAL element at touch coordinates
        val touch = e.touches(0)
        val actualTarget = dom.document.elementFromPoint(touch.clientX, touch.clientY)
        val isInteractive = actualTarget != null && (
          actualTarget.tagName == "BUTTON" ||
          actualTarget.tagName == "A" ||
          actualTarget.tagName == "INPUT" ||
          actualTarget.closest("button, a, input") != null
        )
        if !isInteractive then
          handleDragStart(touch.clientX, e.currentTarget.asInstanceOf[dom.Element])
      },
      onTouchMove --> { (e: dom.TouchEvent) =>
        val touch = e.touches(0)
        handleDragMove(touch.clientX)
        // Prevent scroll while swiping
        if swipeState.now().isDragging && math.abs(swipeState.now().offsetX) > 10 then
          e.preventDefault()
      },
      onTouchEnd --> { (_: dom.TouchEvent) =>
        handleDragEnd()
      },
      onTouchCancel --> { (_: dom.TouchEvent) =>
        isAnimating.set(true)
        swipeState.set(SwipeState())
        val _ = window.setTimeout(() => isAnimating.set(false), 300)
      },
      // Mouse events for desktop
      onMouseDown --> { (e: dom.MouseEvent) =>
        // Use elementFromPoint to get the ACTUAL element at click coordinates
        // This works around CSS Grid capturing events at the wrong level
        val actualTarget = dom.document.elementFromPoint(e.clientX, e.clientY)
        val isInteractive = actualTarget != null && (
          actualTarget.tagName == "BUTTON" ||
          actualTarget.tagName == "A" ||
          actualTarget.tagName == "INPUT" ||
          actualTarget.closest("button, a, input") != null
        )
        if !isInteractive then
          e.preventDefault()
          handleDragStart(e.clientX, e.currentTarget.asInstanceOf[dom.Element])
      },
      windowEvents(_.onMouseMove) --> { (e: dom.MouseEvent) =>
        handleDragMove(e.clientX)
      },
      windowEvents(_.onMouseUp) --> { (_: dom.MouseEvent) =>
        if swipeState.now().isDragging then
          handleDragEnd()
      },
      cardContent,
    ),
    // Threshold indicator (subtle line that appears when close)
    div(
      cls := "SwipeThresholdIndicator SwipeThresholdIndicator--left",
      opacity <-- swipeState.signal.map { state =>
        if state.offsetX < 0 && math.abs(state.progress) > 0.7 then 
          (math.abs(state.progress) - 0.7) / 0.3
        else 0
      },
    ),
    div(
      cls := "SwipeThresholdIndicator SwipeThresholdIndicator--right",
      opacity <-- swipeState.signal.map { state =>
        if state.offsetX > 0 && state.progress > 0.7 then 
          (state.progress - 0.7) / 0.3
        else 0
      },
    ),
  )

private def SingleDiscussionComponent(
  name: StrictSignal[Person],
  topicUpdates: DiscussionAction => Unit,
  signal: Signal[Option[Discussion]],
  transition: Option[Transition] = None,
  enableSwipe: Boolean = true,
  iconModifiers: Seq[Modifier[HtmlElement]] = Seq.empty,
) = {
  signal.map {
    case Some(topic) =>
      // Heat level based on votes (accessible: uses color + border + icon)
      val votes = topic.votes
      val heatLevel = 
        if (votes >= 5) "heat-hot"
        else if (votes >= 3) "heat-warm"  
        else if (votes >= 1) "heat-mild"
        else "heat-cold"
      
      // Background color based on the user's personal vote
      val currentUserFeedback = topic.interestedParties.find(_.voter == name.now())
      val backgroundColorByPosition = currentUserFeedback match
        case Some(feedback) if feedback.position == VotePosition.Interested => "#d4edda"    // green - interested
        case Some(feedback) if feedback.position == VotePosition.NotInterested => "#e2e3e5" // gray - not interested
        case _ => "#f8f9fa"                                                                  // neutral - no vote
      
      // Determine vote status (both show in same position on left)
      val voteStatus = currentUserFeedback.map(_.position)
      val hasVoted = currentUserFeedback.isDefined

      val cardContent = div(
        cls := s"TopicCard $heatLevel", // Heat level class for visual indicator
        // Celebration animation class when vote is confirmed
        cls <-- FrontEnd.celebratingTopics.signal.map { celebrating =>
          celebrating.get(topic.id) match
            case Some(VotePosition.Interested) => "vote-celebrating vote-celebrating--interested"
            case Some(VotePosition.NotInterested) => "vote-celebrating vote-celebrating--notinterested"
            case None => ""
        },
        backgroundColor := backgroundColorByPosition,
        transition match
          case Some(value) => value.height
          case None        => cls:="not-animating-anymore"
        ,
        // Particle elements for celebration effect
        children <-- FrontEnd.celebratingTopics.signal.map { celebrating =>
          if celebrating.contains(topic.id) then
            Seq(
              div(
                cls := "vote-particles",
                (1 to 6).map(_ => div(cls := "vote-particle")),
              )
            )
          else Seq.empty
        },
        // Vote status indicator (unified left position) - shows icon + vote count
        div(
          cls := (voteStatus match
            case Some(VotePosition.Interested) => "VoteIndicator VoteIndicator--interested VoteIndicator--visible"
            case Some(VotePosition.NotInterested) => "VoteIndicator VoteIndicator--notinterested VoteIndicator--visible"
            case None => "VoteIndicator"
          ),
          // Icon based on vote type
          span(
            cls := "VoteIndicator-icon",
            voteStatus match
              case Some(VotePosition.Interested) => "♥"
              case Some(VotePosition.NotInterested) => "✗"
              case None => ""
          ),
          // Vote count (only shown after voting)
          if hasVoted then
            span(cls := "VoteIndicator-count", votes.toString)
          else
            span(),
        ),
        div(
          cls := "MainActive",
          // Inline editable topic name (only for the facilitator)
          InlineEditableTitle(
            topic,
            name.now(),
            newTitle => {
              if connectionStatus.checkReady() then
                Topic.make(newTitle) match
                  case Right(validTopic) =>
                    topicUpdates(DiscussionAction.Rename(topic.id, validTopic))
                  case Left(_) => () // Invalid topic name, ignore
            },
            () => {
              if connectionStatus.checkReady() then
                topicUpdates(DiscussionAction.Delete(topic.id))
            }
          ),
        ),
        div(
          cls := "SecondaryActive",
          span(
            GitHubAvatar(topic.facilitator).amend(iconModifiers*),
          ),
          span(
            cls := "FacilitatorName",
            topic.facilitatorName,
          ),
          topic.roomSlot match {
            case Some(roomSlot) =>
              span(
                cls := "RoomSlot",
                roomSlot.displayString,
              )
            case None =>
              span(
                cls := "RoomSlot RoomSlot--unscheduled",
                "Unscheduled",
              )
          },
          topic.slackThreadUrl match {
            case Some(url) =>
              a(
                href := url,
                target := "_blank",
                cls := "SlackThreadLink",
                title := "Discuss in Slack",
                img(src := "/icons/slack.svg", cls := "SlackIcon"),
              )
            case None => span()
          },
        ),
      )
      
      // Wrap with swipe functionality if enabled
      if enableSwipe then
        SwipeableCard(topic, name, topicUpdates, cardContent)
      else
        cardContent
        
    case None =>
      div("nothing")
  }

}

private def DiscussionSubview(
  topicsOfInterest: Signal[List[Discussion]],
  votePosition: Option[VotePosition],
  name: StrictSignal[Person],
  topicUpdates: DiscussionAction => Unit,
  updateTargetDiscussion: Observer[Discussion],
  firstUnjudgedId: Signal[Option[TopicId]] = Signal.fromValue(None),
  showSwipeHint: Signal[Boolean] = Signal.fromValue(false),
) =
  div(
    cls := "TopicsContainer",
    children <--
      topicsOfInterest
        .splitTransition(_.id)(
          (
            index: TopicId,
            topic: Discussion,
            signal: Signal[Discussion],
            transition: Transition,
          ) =>
            // Check if this is the first unjudged card (for wiggle hint)
            val isFirstUnjudged = firstUnjudgedId.map(_.contains(topic.id))
            val shouldWiggle = isFirstUnjudged.combineWith(showSwipeHint).map {
              case (first, hint) => first && hint
            }
            
            div(
              // Apply wiggle class to the wrapper when needed
              cls <-- shouldWiggle.map(if _ then "SwipeHintWiggle" else ""),
              child <-- SingleDiscussionComponent(
                name,
                topicUpdates,
                signal.map(Some(_)),
                Some(transition),
              ),
            ),
        ),
  )

/** Linear schedule view - shows full topic cards in a vertical list
  * organized by day, time slot, and room.
  */
def LinearScheduleView(
  $discussionState: Signal[DiscussionState],
  topicUpdates: DiscussionAction => Unit,
  name: StrictSignal[Person],
  unscheduledMenuState: Var[Option[RoomSlot]],
) =
  val showUnscheduledMenu: Observer[RoomSlot] =
    Observer { roomSlot =>
      unscheduledMenuState.set(Some(roomSlot))
    }

  div(
    cls := "LinearScheduleView",
    children <-- $discussionState.map { state =>
      state.slots.map { daySlot =>
        div(
          cls := "LinearDay",
          div(cls := "LinearDayHeader", daySlot.date.getDayOfWeek.toString),
          daySlot.slots.map { timeSlotForAllRooms =>
            div(
              cls := "LinearTimeSlot",
              div(cls := "LinearTimeHeader", timeSlotForAllRooms.time.s),
              timeSlotForAllRooms.rooms.map { room =>
                val roomSlot = RoomSlot(room, timeSlotForAllRooms.time)
                val discussion = state.roomSlotContent(roomSlot)
                div(
                  cls := "LinearRoomSlot",
                  div(cls := "LinearRoomName", room.name),
                  discussion match {
                    case Some(disc) =>
                      div(
                        child <-- SingleDiscussionComponent(
                          name,
                          topicUpdates,
                          Signal.fromValue(Some(disc)),
                        ),
                      )
                    case None =>
                      div(
                        cls := "LinearEmptySlot",
                        cursor := "pointer",
                        onClick.stopPropagation.mapTo(roomSlot) --> showUnscheduledMenu,
                        SvgIcon(GlyphiconUtils.plus),
                        span("Add topic"),
                      )
                  },
                )
              },
            )
          },
        )
      }
    },
  )

/** Reusable voting buttons component - matches the Topics view styling */
def VoteButtons(
  discussion: Discussion,
  name: StrictSignal[Person],
  topicUpdates: DiscussionAction => Unit,
) =
  val currentFeedback =
    discussion.interestedParties.find(_.voter == name.now())
  val isInterested =
    currentFeedback.exists(_.position == VotePosition.Interested)
  val isNotInterested =
    currentFeedback.exists(_.position == VotePosition.NotInterested)
  val votes = discussion.votes
  val heatLevel = 
    if (votes >= 5) "heat-hot"
    else if (votes >= 3) "heat-warm"  
    else if (votes >= 1) "heat-mild"
    else "heat-cold"
  
  def handleVote(target: VotePosition): Unit =
    // Only allow voting if connection is ready (prevents actions during reconnect/sync)
    if connectionStatus.checkReady() then
      val voter = name.now()
      val currentPosition = currentFeedback.map(_.position)
      // Only update if switching to a different position (no going back to neutral)
      if !currentPosition.contains(target) then
        topicUpdates(DiscussionAction.Vote(discussion.id, Feedback(voter, target)))
    else
      println("Connection not ready, ignoring vote action")

  div(
    cls := "VoteButtonRow",
    button(
      cls := (
        if isInterested then "VoteButton VoteButton--interested VoteButton--active"
        else "VoteButton VoteButton--interested"
      ),
      onClick --> Observer { _ =>
        handleVote(VotePosition.Interested)
      },
      SvgIcon(GlyphiconUtils.heart, "VoteIcon"),
    ),
    if (currentFeedback.isDefined) {
      span(
        cls := s"VoteCount $heatLevel",
        if (votes >= 5) "🔥 " else if (votes >= 3) "♨️ " else "",
        votes.toString,
      )
    } else {
      span(cls := "VoteCount VoteCount--hidden", "?")
    },
    button(
      cls := (
        if isNotInterested then "VoteButton VoteButton--notinterested VoteButton--active"
        else "VoteButton VoteButton--notinterested"
      ),
      onClick --> Observer { _ =>
        handleVote(VotePosition.NotInterested)
      },
      SvgIcon(GlyphiconUtils.noSymbol, "VoteIcon"),
    ),
  )

enum AppView:
  case Admin
  case Topics
  case Schedule

/** Segmented control for switching between views.
  * Modern pill-style toggle that's obvious on mobile.
  * Admin tab is only visible when admin mode is enabled.
  */
def ViewToggle(
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

def ScheduleSlotComponent(
  roomSlot: RoomSlot,
  $slotContent: Signal[Option[Discussion]],
  updateDiscussion: Observer[Discussion],
  $activeDiscussion: StrictSignal[Option[Discussion]],
  showPopover: Observer[Discussion],
  showSwapMenu: Observer[(Discussion, Discussion)],
  showUnscheduledMenu: Observer[RoomSlot],
  topicUpdates: DiscussionAction => Unit,
  name: StrictSignal[Person],
) =
  // Keep unregister handle across mount/unmount
  var unregisterSlot: () => Unit = () => ()
  span(
    // Track this slot's DOM position so move animations can start from the
    // actual on-screen slot instead of an approximate grid offset
    onMountUnmountCallback(
      ctx =>
        val element = ctx.thisNode.ref
        unregisterSlot =
          SlotPositionTracker.register(roomSlot, element)
      ,
      _ => unregisterSlot(),
    ),
    // Combine slot content with active discussion to render appropriately
    child <-- $slotContent.combineWith($activeDiscussion).map { case (slotContentOpt, discussionO) =>
      slotContentOpt match
        case Some(value) =>
          val selectedTopicStyling =
            if ($activeDiscussion.now().map(_.id).contains(value.id))
              "activeTopicIcon"
            else ""
          // Check if there's an active discussion that's different from this slot's topic
          val isSwappable = discussionO.exists(active =>
            active.id != value.id && active.roomSlot.isDefined,
          )

          // Get the animated offset for this topic (if it's part of a swap)
          val $offset = SwapAnimationState.getOffsetSignal(value.id)
          // Use spring animation for smooth movement
          val $animatedX = $offset.map(_._1).spring
          val $animatedY = $offset.map(_._2).spring

          // Calculate voting state for styling
          val votingState = VotingState.forUser(value, name.now())

          span(
            cls := "swap-topic-icon",
            // Apply animated transform based on swap offset
            transform <-- $animatedX.combineWith($animatedY).map {
              case (x, y) =>
                if (x != 0.0 || y != 0.0) s"translate(${x}px, ${y}px)"
                else "none"
            },
            onClick.stopPropagation.mapTo(value) --> showPopover,
            // Long-press to show swap menu when there's an active discussion
            if (isSwappable)
              onContextMenu.preventDefault --> Observer { (event: org.scalajs.dom.MouseEvent) =>
                event.stopPropagation()
                discussionO.foreach { activeDiscussion =>
                  showSwapMenu.onNext((activeDiscussion, value))
                }
              }
            else emptyMod,
            onClick.mapTo(value) --> updateDiscussion,
            GitHubAvatar.withVotingState(
              value.facilitator,
              votingState,
              value.topicName,
              s"filledTopic $selectedTopicStyling",
            ),
          )
        case None =>
          discussionO match
            case Some(discussion) =>
              discussion.roomSlot match
                case Some(value) if roomSlot == value =>
                  GitHubAvatar(discussion.facilitator, "filledTopic")
                case Some(_) =>
                  // Empty slot when active discussion is scheduled elsewhere
                  span(
                    cls := "emptySlotWithActiveDiscussion",
                    SvgIcon(GlyphiconUtils.emptySlot),
                    onClick.stopPropagation.mapTo(roomSlot) --> showUnscheduledMenu,
                  )
                case None =>
                  // Empty slot when active discussion is unscheduled
                  span(
                    cls := "emptySlotWithActiveDiscussion",
                    SvgIcon(GlyphiconUtils.plus),
                    onClick.stopPropagation.mapTo(roomSlot) --> showUnscheduledMenu,
                  )
            case None =>
              // Empty slot with no active discussion - show menu on click
              span(
                SvgIcon(GlyphiconUtils.emptySlot),
                onClick.stopPropagation.mapTo(roomSlot) --> showUnscheduledMenu,
              )
    },
  )

def SlotSchedule(
  $discussionState: Signal[DiscussionState],
  timeSlotsForAllRooms: TimeSlotForAllRooms,
  updateDiscussion: Observer[Discussion],
  activeDiscussion: StrictSignal[Option[Discussion]],
  showPopover: Observer[Discussion],
  showSwapMenu: Observer[(Discussion, Discussion)],
  showUnscheduledMenu: Observer[RoomSlot],
  topicUpdates: DiscussionAction => Unit,
  name: StrictSignal[Person],
) =
  div(
    cls := "SlotRow",
    div(cls := "TimeOfSlot", timeSlotsForAllRooms.time.s),
    timeSlotsForAllRooms.rooms.map { room =>
      val roomSlot = RoomSlot(room, timeSlotsForAllRooms.time)
      val $slotContent = $discussionState.map(_.roomSlotContent(roomSlot))
      div(
        cls := "Cell",
        ScheduleSlotComponent(
          roomSlot,
          $slotContent,
          updateDiscussion,
          activeDiscussion,
          showPopover,
          showSwapMenu,
          showUnscheduledMenu,
          topicUpdates,
          name,
        ),
      )
    },
  )

/** Inline editable title for topic cards.
  * Shows as plain text with an edit button for the facilitator.
  */
def InlineEditableTitle(
  topic: Discussion,
  currentUser: Person,
  onRename: String => Unit,
  onDelete: () => Unit,
): HtmlElement =
  val isEditing = Var(false)
  val editValue = Var(topic.topicName)
  val canEdit = topic.facilitator == currentUser
  
  def saveAndClose(): Unit =
    val newValue = editValue.now().trim
    if newValue.nonEmpty && newValue != topic.topicName then
      onRename(newValue)
    isEditing.set(false)
  
  def cancelEdit(): Unit =
    editValue.set(topic.topicName)
    isEditing.set(false)
  
  def startEditing(e: dom.MouseEvent): Unit =
    e.stopPropagation()
    e.preventDefault()
    editValue.set(topic.topicName)
    isEditing.set(true)
  
  div(
    cls := "InlineEditableTitle",
    child <-- isEditing.signal.map { editing =>
      if editing then
        div(
          cls := "InlineEditableTitle-editRow",
          input(
            cls := "InlineEditableTitle-input",
            typ := "text",
            value <-- editValue.signal,
            onInput.mapToValue --> editValue,
            onBlur --> Observer(_ => saveAndClose()),
            onKeyDown --> Observer { (e: dom.KeyboardEvent) =>
              e.key match
                case "Enter" => saveAndClose()
                case "Escape" => cancelEdit()
                case _ => ()
            },
            // Prevent swipe from capturing input interactions
            onMouseDown --> Observer { (e: dom.MouseEvent) => e.stopPropagation() },
            onTouchStart --> Observer { (e: dom.TouchEvent) => e.stopPropagation() },
            onMountCallback { ctx =>
              val el = ctx.thisNode.ref.asInstanceOf[dom.html.Input]
              el.focus()
              // Put cursor at end of text (not selecting all)
              val len = el.value.length
              el.setSelectionRange(len, len)
            },
          ),
          a(
            cls := "InlineEditableTitle-cancelBtn",
            href := "#",
            onClick --> Observer { (e: dom.MouseEvent) =>
              e.preventDefault()
              e.stopPropagation()
              cancelEdit()
            },
            "✕",
          ),
        )
      else
        div(
          cls := "InlineEditableTitle-displayRow",
          span(
            cls := "InlineEditableTitle-text",
            topic.topicName,
          ),
          // Edit and delete buttons only shown for facilitator
          if canEdit then
            span(
              cls := "InlineEditableTitle-actions",
              a(
                cls := "InlineEditableTitle-editBtn",
                href := "#",
                title := "Edit title",
                onClick --> Observer { (e: dom.MouseEvent) => 
                  e.preventDefault()
                  startEditing(e) 
                },
                "✎",
              ),
              a(
                cls := "InlineEditableTitle-deleteBtn",
                href := "#",
                title := "Delete topic",
                onClick --> Observer { (e: dom.MouseEvent) => 
                  e.preventDefault()
                  e.stopPropagation()
                  if dom.window.confirm(s"Delete topic '${topic.topicName}'? This cannot be undone.") then
                    onDelete()
                },
                "🗑",
              ),
            )
          else
            span()
          ,
        )
    },
  )

/** Error banner that displays user-visible errors.
  * Merges local errors with connectionStatus.userError for unified error display.
  */
case class ErrorBanner(
  localError: Var[Option[String]] = Var(None)):
  
  /** Set a local error (for validation, etc.) */
  def setError(msg: String): Unit = localError.set(Some(msg))
  
  /** Clear local error */
  def clearError(): Unit = localError.set(None)
  
  /** Observer for setting errors */
  val errorObserver: Observer[Option[String]] = Observer { msg =>
    localError.set(msg)
  }
  
  val component =
    div(
      child <--
        // Merge local errors with connectionStatus errors
        localError.signal.combineWith(connectionStatus.userError.signal).map {
          case (Some(msg), _) =>
            // Local error takes precedence
            div(
              cls := "ErrorBanner",
              span(cls := "ErrorBanner-message", "Error: " + msg),
              button(
                cls := "ErrorBanner-dismiss",
                onClick --> Observer(_ => localError.set(None)),
                "×",
              ),
            )
          case (None, Some(msg)) =>
            // Show connection status error
            div(
              cls := "ErrorBanner",
              span(cls := "ErrorBanner-message", "Error: " + msg),
              button(
                cls := "ErrorBanner-dismiss",
                onClick --> Observer(_ => connectionStatus.clearError()),
                "×",
              ),
            )
          case (None, None) =>
            div()
        },
    )

def Menu(
  selectedDiscussion: Discussion,
  targetDiscussion: Discussion,
  topicUpdates: DiscussionAction => Unit,
  dismissMenu: Observer[Unit],
) =
  val (x, y) = MenuPositioning.standardMenuPosition()
  div(
    cls := "Menu",
    left := s"${x}px",
    top := s"${y}px",
    onClick.preventDefault.stopPropagation --> Observer(_ => ()),
    div(cls := "Menu-header", "Actions"),
    // Selected topic (current selection)
    div(
      cls := "Menu-section",
      div(cls := "Menu-label", "Selected Topic:"),
      div(
        cls := "Menu-topic Menu-topic--selected",
        GitHubAvatar(selectedDiscussion.facilitator),
        div(
          div(cls := "Menu-topicName", selectedDiscussion.topicName),
          div(
            cls := "Menu-roomSlot",
            selectedDiscussion.roomSlot
              .map(_.displayString)
              .getOrElse("Unscheduled"),
          ),
        ),
      ),
    ),
    // Target topic
    div(
      cls := "Menu-section Menu-section--target",
      div(cls := "Menu-label", "Target Topic:"),
      div(
        cls := "Menu-topic Menu-topic--target",
        GitHubAvatar(targetDiscussion.facilitator),
        div(
          div(cls := "Menu-topicName", targetDiscussion.topicName),
          div(
            cls := "Menu-roomSlot",
            targetDiscussion.roomSlot
              .map(_.displayString)
              .getOrElse("Unscheduled"),
          ),
        ),
      ),
    ),
    // Action buttons
    div(
      cls := "Menu-actions",
      button(
        cls := "Menu-swapButton",
        onClick --> Observer { _ =>
          // Both discussions must have room slots for swap to work
          (selectedDiscussion.roomSlot,
           targetDiscussion.roomSlot,
          ) match
            case (Some(slot1), Some(slot2)) =>
              topicUpdates(
                DiscussionAction.SwapTopics(
                  selectedDiscussion.id,
                  slot1,
                  targetDiscussion.id,
                  slot2,
                ),
              )
              dismissMenu.onNext(())
            case _ => () // Should not happen - UI prevents this
        },
        span("⇅"),
        span("Swap Room Slots"),
      ),
      button(
        cls := "Menu-cancelButton",
        onClick.mapToUnit --> dismissMenu,
        "Cancel",
      ),
    ),
  )

def UnscheduledDiscussionsMenu(
  unscheduledDiscussions: List[Discussion],
  targetRoomSlot: RoomSlot,
  facilitator: StrictSignal[Person],
  topicUpdates: DiscussionAction => Unit,
  dismissMenu: Observer[Unit],
  setActiveDiscussion: Observer[Discussion],
  activeDiscussion: Option[Discussion],
  currentView: AppView,
) =
  val (x, y) = MenuPositioning.standardMenuPosition()
  val textVar = Var("")
  val errorVar = Var[Option[String]](None)

  def submitNewDiscussion() =
    val topicAttempt =
      Topic.make(textVar.now())

    topicAttempt match
      case Left(error) =>
        errorVar.set(Some(error))
      case Right(topic) =>
        val facilitatorName = facilitator.now()
        if (facilitatorName.unwrap.trim.length < 2) {
          errorVar.set(
            Some(
              "Please enter your name (2+ characters) before adding.",
            ),
          )
        }
        else {
          topicUpdates(
            DiscussionAction.AddWithRoomSlot(
              topic,
              facilitatorName,
              targetRoomSlot,
            ),
          )
          textVar.set("")
          errorVar.set(None)
          dismissMenu.onNext(())
        }

  // Determine if we should show the "Move current topic here" option
  // Only show if:
  // 1. We're on the Admin view (where the selected topic is visible)
  // 2. There's an active discussion that could be moved to this slot
  val moveCurrentTopicOption: Option[HtmlElement] = 
    if (currentView != AppView.Admin) None
    else activeDiscussion.flatMap { discussion =>
      // Only show if the active discussion is not already in this slot
      if (discussion.roomSlot.contains(targetRoomSlot)) None
      else
        Some(
          div(
            cls := "Menu-section",
            div(cls := "Menu-label", "Move Current Topic Here:"),
            div(
              cls := "Menu-actions",
              button(
                cls := "Menu-swapButton",
                onClick --> Observer { _ =>
                  topicUpdates(
                    DiscussionAction.MoveTopic(
                      discussion.id,
                      targetRoomSlot,
                    ),
                  )
                  dismissMenu.onNext(())
                },
                GitHubAvatar(discussion.facilitator),
                span(
                  cls := "Menu-topicName",
                  discussion.topicName,
                ),
              ),
            ),
          ),
        )
  }

  div(
    cls := "Menu",
    left := s"${x}px",
    top := s"${y}px",
    onClick.preventDefault.stopPropagation --> Observer(_ => ()),
    div(cls := "Menu-header", "Assign Discussion"),
    div(
      cls := "Menu-section",
      div(cls := "Menu-label",
          s"Room Slot: ${targetRoomSlot.displayString}",
      ),
    ),
    // Move current topic option (if applicable)
    moveCurrentTopicOption.getOrElse(span()),
    div(
      cls := "Menu-section",
      div(cls := "Menu-label", "Create New Discussion:"),
      textArea(
        cls := "Menu-textArea",
        placeholder := "Describe the discussion to schedule...",
        value <-- textVar.signal,
        onInput.mapToValue --> textVar,
        onMountCallback(ctx => ctx.thisNode.ref.focus()),
      ),
      button(
        cls := "Menu-swapButton",
        onClick --> Observer { _ =>
          submitNewDiscussion()
        },
        "Add & Assign",
      ),
      child <--
        errorVar.signal.map {
          case Some(errorMessage) =>
            div(cls := "Menu-error", errorMessage)
          case None =>
            span()
        },
    ),
    div(
      cls := "Menu-section",
      div(cls := "Menu-label", "Move Existing Unscheduled Topic:"),
      if (unscheduledDiscussions.isEmpty) {
        div(
          cls := "Menu-topic",
          div(
            div(cls := "Menu-topicName",
                "No unscheduled discussions available",
            ),
          ),
        )
      }
      else {
        div(
          cls := "Menu-actions",
          unscheduledDiscussions.map { discussion =>
            button(
              cls := "Menu-swapButton",
              onClick --> Observer { _ =>
                topicUpdates(
                  DiscussionAction.UpdateRoomSlot(
                    discussion.id,
                    targetRoomSlot,
                  ),
                )
                // Set the discussion as active after assigning it to the room slot
                setActiveDiscussion.onNext(
                  discussion.copy(roomSlot = Some(targetRoomSlot)),
                )
                dismissMenu.onNext(())
              },
              GitHubAvatar(discussion.facilitator),
              span(
                cls := "Menu-topicName",
                discussion.topicName,
              ),
            )
          },
        )
      },
    ),
    div(
      cls := "Menu-actions",
      button(
        cls := "Menu-cancelButton",
        onClick.mapToUnit --> dismissMenu,
        "Cancel",
      ),
    ),
  )

def ActiveDiscussionActionMenu(
  discussion: Discussion,
  topicUpdates: DiscussionAction => Unit,
  dismissMenu: Observer[Unit],
) =
  val (x, y) = MenuPositioning.standardMenuPosition()
  val isScheduled = discussion.roomSlot.isDefined

  val cancelButton =
    button(
      cls := "Menu-cancelButton",
      onClick.mapToUnit --> dismissMenu,
      "Close",
    )

  val actionElements: List[Modifier[HtmlElement]] =
    if isScheduled then
      List(
        button(
          cls := "Menu-swapButton",
          onClick --> Observer { _ =>
            topicUpdates(
              DiscussionAction.Unschedule(discussion.id),
            )
            dismissMenu.onNext(())
          },
          SvgIcon(GlyphiconUtils.minus),
          span("Unschedule topic"),
        ),
        cancelButton,
      )
    else
      List(
        div(
          cls := "Menu-topic Menu-topic--selected",
          span(
            cls := "Menu-topicName",
            "This discussion is not scheduled yet.",
          ),
        ),
        cancelButton,
      )

  val actionSection =
    (cls := "Menu-actions") :: actionElements

  div(
    cls := "Menu",
    left := s"${x}px",
    top := s"${y}px",
    onClick.preventDefault.stopPropagation --> Observer(_ => ()),
    div(cls := "Menu-header", "Discussion actions"),
    div(
      cls := "Menu-topic Menu-topic--selected",
      GitHubAvatar(discussion.facilitator),
      div(
        div(
          cls := "Menu-topicName",
          discussion.topicName,
        ),
        div(
          cls := "Menu-roomSlot",
          discussion.roomSlot
            .map(_.displayString)
            .getOrElse("Unscheduled"),
        ),
      ),
    ),
    div(actionSection*),
  )

private def activeDiscussionLongPressBinder(
  activeDiscussionNow: () => Option[Discussion],
  showActiveDiscussionMenu: Observer[Discussion],
): Binder[HtmlElement] =
  onContextMenu.preventDefault --> Observer {
    (event: org.scalajs.dom.MouseEvent) =>
      event.stopPropagation()
      activeDiscussionNow().foreach { discussion =>
        if (discussion.roomSlot.isDefined) {
          showActiveDiscussionMenu.onNext(discussion)
        }
      }
  }

def ScheduleView(
  fullSchedule: Var[DiscussionState],
  activeDiscussion: Var[Option[Discussion]],
  topicUpdates: DiscussionAction => Unit,
  name: StrictSignal[Person],
  updateTargetDiscussion: Observer[Discussion],
  popoverState: Var[Option[Discussion]],
  swapMenuState: Var[Option[(Discussion, Discussion)]],
  unscheduledMenuState: Var[Option[RoomSlot]],
  activeDiscussionMenuState: Var[Option[Discussion]],
) =
  val showPopover: Observer[Discussion] =
    Observer { discussion =>
      popoverState.set(Some(discussion))
    }

  val showSwapMenu: Observer[(Discussion, Discussion)] =
    Observer { case (selected, target) =>
      swapMenuState.set(Some((selected, target)))
    }

  val showUnscheduledMenu: Observer[RoomSlot] =
    Observer { roomSlot =>
      unscheduledMenuState.set(Some(roomSlot))
    }

  val showActiveDiscussionMenu: Observer[Discussion] =
    Observer { discussion =>
      activeDiscussionMenuState.set(Some(discussion))
    }

  val handleActiveDiscussionLongPress =
    activeDiscussionLongPressBinder(() => activeDiscussion.now(),
                                    showActiveDiscussionMenu,
    )

  div(
    cls := "container",
    div(
      cls := "Targets",
      div(
        cls := "ActiveDiscussion Topic",
        handleActiveDiscussionLongPress,
        child <-- SingleDiscussionComponent(
          name,
          topicUpdates,
          activeDiscussion.signal,
          iconModifiers = Seq(handleActiveDiscussionLongPress),
        ),
      ),
    ),
    div(
      cls := "Schedule",
      div(
        cls := "RoomHeaders",
        div(cls := "Room1", "King"),
        div(cls := "Room2", "Hawk"),
        div(cls := "Room3", "Art"),
        div(cls := "Room4", "Dance"),
      ),
      div(
        cls := "TimeSlots",
        ScrollPreserver.timeSlotsIdAttr,
        SlotSchedules(
          fullSchedule.signal,
          updateTargetDiscussion,
          activeDiscussion.signal,
          showPopover,
          showSwapMenu,
          showUnscheduledMenu,
          topicUpdates,
          name,
        ),
      ),
    ),
  )

def SlotSchedules(
  $discussionState: Signal[DiscussionState],
  updateDiscussion: Observer[Discussion],
  activeDiscussion: StrictSignal[Option[Discussion]],
  showPopover: Observer[Discussion],
  showSwapMenu: Observer[(Discussion, Discussion)],
  showUnscheduledMenu: Observer[RoomSlot],
  topicUpdates: DiscussionAction => Unit,
  name: StrictSignal[Person],
) =
  // Build the grid structure statically - slots don't change, only their content does
  val slots = DiscussionState.timeSlotExamples
  div(
    slots.map { daySlot =>
      div(
        div(cls := "DayHeader", daySlot.date.getDayOfWeek.toString().take(3)),
        daySlot.slots.map { timeSlotsForAllRooms =>
          div(
            cls := "SlotRow",
            div(cls := "TimeOfSlot", timeSlotsForAllRooms.time.s),
            timeSlotsForAllRooms.rooms.map { room =>
              val roomSlot = RoomSlot(room, timeSlotsForAllRooms.time)
              // Derive a signal that only emits when THIS slot's content changes
              val $slotContent = $discussionState.map(_.roomSlotContent(roomSlot))
              div(
                cls := "Cell",
                ScheduleSlotComponent(
                  roomSlot,
                  $slotContent,
                  updateDiscussion,
                  activeDiscussion,
                  showPopover,
                  showSwapMenu,
                  showUnscheduledMenu,
                  topicUpdates,
                  name,
                ),
              )
            },
          )
        },
      )
    },
  )

def deleteCookie(
  name: String,
) =
  dom.document.cookie =
    name + "=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/;";

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
    case DiscussionActionConfirmed.UpdateRoomSlot(topicId, _) =>
      (Some(topicId), false)
    case DiscussionActionConfirmed.Unschedule(topicId) =>
      (Some(topicId), false)
    case DiscussionActionConfirmed.MoveTopic(topicId, _) =>
      (Some(topicId), false)
    case DiscussionActionConfirmed.SwapTopics(topic1, _, _, _) =>
      (Some(topic1), false)
    case DiscussionActionConfirmed.AddResult(_) =>
      (None, false)
    case DiscussionActionConfirmed.SlackThreadLinked(topicId, _) =>
      (Some(topicId), false)
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
val connectionStatus = new ConnectionStatusManager(
  topicUpdates,
  MaxReconnectRetries,
)

/** Fetch a ticket, automatically refreshing the access token if needed */
def fetchTicketWithRefresh(): EventStream[String] = {
  import scala.concurrent.ExecutionContext.Implicits.global
  
  // Check if token needs refresh before making the request
  if (isAccessTokenExpired) {
    EventStream.fromFuture(
      refreshAccessToken().flatMap { refreshed =>
        if (refreshed) {
          // Token refreshed, now fetch the ticket
          dom.fetch("/ticket", new dom.RequestInit {
            method = dom.HttpMethod.GET
            headers = new dom.Headers(scalajs.js.Array(
              scalajs.js.Array("Authorization", s"Bearer ${getCookie("access_token").getOrElse("")}")
            ))
          }).toFuture.flatMap(_.text().toFuture)
        } else {
          // Refresh failed, redirect to login
          window.location.href = "/auth"
          scala.concurrent.Future.failed(new Exception("Token refresh failed"))
        }
      }
    )
  } else {
    // Token is still valid, fetch directly
    FetchStream.get(
      "/ticket",
      fetchOptions =>
        fetchOptions.headers(
          "Authorization" -> s"Bearer ${getCookie("access_token").get}",
        ),
    )
  }
}

/** Fetch a fresh auth ticket, refreshing access token if needed.
  * Returns a Future that resolves to the ticket response text.
  */
private def fetchTicketAsync(): scala.concurrent.Future[String] =
  if isAccessTokenExpired then
    println("Access token expired, refreshing...")
    refreshAccessToken().flatMap { refreshed =>
      if refreshed then
        dom.fetch("/ticket", new dom.RequestInit {
          method = dom.HttpMethod.GET
          headers = new dom.Headers(scalajs.js.Array(
            scalajs.js.Array("Authorization", s"Bearer ${getCookie("access_token").getOrElse("")}")
          ))
        }).toFuture.flatMap { response =>
          if response.ok then response.text().toFuture
          else if response.status == 401 then
            scala.concurrent.Future.failed(new Exception("Unauthorized - please log in again"))
          else
            scala.concurrent.Future.failed(new Exception(s"HTTP ${response.status}"))
        }
      else
        scala.concurrent.Future.failed(new Exception("Token refresh failed"))
    }
  else
    dom.fetch("/ticket", new dom.RequestInit {
      method = dom.HttpMethod.GET
      headers = new dom.Headers(scalajs.js.Array(
        scalajs.js.Array("Authorization", s"Bearer ${getCookie("access_token").getOrElse("")}")
      ))
    }).toFuture.flatMap { response =>
      if response.ok then response.text().toFuture
      else if response.status == 401 then
        scala.concurrent.Future.failed(new Exception("Unauthorized - please log in again"))
      else
        scala.concurrent.Future.failed(new Exception(s"HTTP ${response.status}"))
    }

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
) =
  // Register the sync operation with the connection manager
  // This will be called on connect, reconnect, and visibility return
  connectionStatus.onSync {
    fetchTicketAsync().map { responseText =>
      responseText.fromJson[Ticket] match
        case Right(ticket) =>
          println("Ticket received, sending to server for state sync")
          topicUpdates.sendOne(ticket)
        case Left(parseError) =>
          throw new Exception(s"Invalid ticket response: $parseError")
    }
  }

  div(
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
    // Handle rejected actions by re-authenticating and retrying
    // Uses connectionStatus.withErrorHandling to catch and report errors
    topicUpdates.received.flatMapSwitch { event =>
      event match
        case DiscussionActionConfirmed.Rejected(discussionAction) =>
          EventStream.fromFuture(
            connectionStatus.withErrorHandling(
              fetchTicketAsync().map { responseText =>
                responseText.fromJson[Ticket] match
                  case Right(ticket) => (ticket, discussionAction)
                  case Left(err) => throw new Exception(s"Failed to parse ticket: $err")
              },
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
