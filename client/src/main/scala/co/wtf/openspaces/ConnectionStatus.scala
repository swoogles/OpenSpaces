package co.wtf.openspaces

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveElement
import io.laminext.websocket.WebSocket
import org.scalajs.dom

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** Represents the WebSocket connection state with all relevant context.
  *
  * This is a client-side only type that provides type-safe access to
  * connection state information for UI rendering.
  */
enum ConnectionState:
  /** Connected and ready to send/receive messages */
  case Connected

  /** Attempting to establish connection */
  case Connecting

  /** Disconnected, but will automatically retry */
  case Reconnecting(attempt: Int, maxAttempts: Int)

  /** Disconnected with no automatic retry (exhausted retries or manual disconnect) */
  case Disconnected

  /** Device is offline (no network connectivity) */
  case Offline

  /** CSS class for styling the connection indicator */
  def cssClass: String = this match
    case Connected                => "connection-connected"
    case Connecting               => "connection-connecting"
    case Reconnecting(_, _)       => "connection-reconnecting"
    case Disconnected             => "connection-disconnected"
    case Offline                  => "connection-offline"

  /** Human-readable status message */
  def message: String = this match
    case Connected                    => "Connected"
    case Connecting                   => "Connecting..."
    case Reconnecting(attempt, max)   => s"Reconnecting (attempt $attempt/$max)..."
    case Disconnected                 => "Disconnected"
    case Offline                      => "No internet connection"

  /** Icon representation (using unicode for simplicity) */
  def icon: String = this match
    case Connected                => "●"  // Solid circle
    case Connecting               => "◐"  // Half circle (animates via CSS)
    case Reconnecting(_, _)       => "◐"  // Half circle
    case Disconnected             => "○"  // Empty circle
    case Offline                  => "◇"  // Diamond (distinct from server issues)

  /** Whether the connection is in a healthy state */
  def isHealthy: Boolean = this == Connected

  /** Whether we should show a prominent warning banner */
  def shouldShowBanner: Boolean = this match
    case Connected  => false
    case Connecting => false  // Brief connecting state doesn't need banner
    case _          => true

/** Tracks whether state sync is in progress.
  * Used to show UI feedback during reconnection sync.
  */
enum SyncState:
  case Idle
  case Syncing
  case Synced
  case Error(errorMsg: String)

  /** Human-readable status message */
  def message: String = this match
    case Idle              => ""
    case Syncing           => "Syncing data..."
    case Synced            => ""
    case Error(msg)        => s"Sync failed: $msg"

  /** Whether sync is in progress */
  def isSyncing: Boolean = this == Syncing

trait ConnectionStatusUI:
  val userErrorSignal: Signal[Option[String]]

  def reportError(message: String): Unit
  def clearError(): Unit
  def checkReady(): Boolean
  
  /** Execute action only if ready, otherwise report error.
    * Returns true if action was executed.
    */
  def withReady(blockedMessage: String)(action: => Unit): Boolean =
    if checkReady() then
      clearError()
      action
      true
    else
      reportError(blockedMessage)
      false

trait ConnectionStatusCoordinator extends ConnectionStatusUI:
  val syncState: Var[SyncState]
  val connectionEnabled: Var[Boolean]
  val closeObserver: Observer[(dom.WebSocket, Boolean)]
  val connectedObserver: Observer[dom.WebSocket]
  val state: Signal[ConnectionState]
  val syncMessage: Signal[String]

  def recordMessageReceived(): Unit
  def setConnected(connected: Boolean): Unit
  def onSync(operation: => Future[Unit]): Unit
  def triggerSync(): Unit
  def markStateSynchronized(): Unit
  def forceReconnect(): Unit
  def withErrorHandling[T](
    operation: => Future[T],
    errorPrefix: String = "Operation failed",
  ): Future[Option[T]]
  def bind[El <: ReactiveElement.Base]: Binder[El]
  def checkReady(): Boolean

/** Centralized manager for WebSocket connection state, sync, and error handling.
  *
  * This consolidates all reconnect/sync logic into one place:
  * - Tracks connection state (connected/connecting/reconnecting/offline)
  * - Manages sync state with automatic retries
  * - Handles visibility changes, online/offline events, health checks
  * - Provides error handling wrappers for async operations
  * - Coordinates forced reconnects
  *
  * @param ws
  *   The laminext WebSocket instance to monitor
  * @param maxReconnectRetries
  *   Maximum number of reconnection attempts (should match WebSocket config)
  * @param staleThresholdMs
  *   Time in milliseconds after which a return to the page triggers reconnection (default: 2 minutes)
  * @param healthCheckIntervalMs
  *   Interval for periodic health checks (default: 30 seconds)
  * @param maxSyncRetries
  *   Maximum number of sync retry attempts
  */
class ConnectionStatusManager[Receive, Send](
  ws: WebSocket[Receive, Send],
  maxReconnectRetries: Int = 10,
  staleThresholdMs: Double = 2 * 60 * 1000,
  healthCheckIntervalMs: Int = 30 * 1000,
  maxSyncRetries: Int = 3,
  syncAckTimeoutMs: Int = 5000,
)(using ec: ExecutionContext)
    extends ConnectionStatusCoordinator:
  
  // ============================================
  // Connection State
  // ============================================
  
  private val reconnectAttemptVar: Var[Int] = Var(0)
  private val isOnlineVar: Var[Boolean] = Var(dom.window.navigator.onLine)
  private val isConnectedVar: Var[Boolean] = Var(false)
  
  // ============================================
  // Sync State
  // ============================================
  
  val syncState: Var[SyncState] = Var(SyncState.Idle)
  private var currentSyncAttempt: Int = 0
  private var awaitingStateReplace: Boolean = false
  private var syncAckTimeoutHandle: Option[Int] = None
  private var syncRetryTimeoutHandle: Option[Int] = None
  private var syncRunId: Long = 0L
  
  // ============================================
  // Reconnect Control
  // ============================================
  
  /** Controls whether the WebSocket should be connected.
    * Toggle off then on to force reconnection with fresh state.
    */
  val connectionEnabled: Var[Boolean] = Var(true)
  
  // ============================================
  // Error Handling
  // ============================================
  
  /** Observable for user-visible errors (validation failures, action rejections, etc.) */
  val userError: Var[Option[String]] = Var(None)
  val userErrorSignal: Signal[Option[String]] = userError.signal
  
  /** Report a user-visible error that should be shown in the ErrorBanner */
  def reportError(message: String): Unit =
    userError.set(Some(message))
  
  /** Clear any displayed error */
  def clearError(): Unit =
    userError.set(None)
  
  // ============================================
  // Visibility & Health Tracking
  // ============================================
  
  private var lastHiddenTime: Option[Double] = None
  private var lastMessageTime: Double = System.currentTimeMillis().toDouble
  private var healthCheckHandle: Option[Int] = None
  
  /** Record that a message was received (call this from received handler) */
  def recordMessageReceived(): Unit =
    lastMessageTime = System.currentTimeMillis().toDouble

  /** Update tracked connection state - call from connected/closed observers */
  def setConnected(connected: Boolean): Unit =
    isConnectedVar.set(connected)
    if !connected then
      invalidateSyncRun()
      syncState.set(SyncState.Idle)
      currentSyncAttempt = 0
  
  /** Whether the WebSocket is currently connected */
  def isConnected: Boolean = isConnectedVar.now()
  
  // ============================================
  // Sync Operations
  // ============================================
  
  private var syncOperation: Option[() => Future[Unit]] = None
  
  /** Register the sync operation to be called on connect/reconnect.
    * This should fetch auth ticket and send to server.
    */
  def onSync(operation: => Future[Unit]): Unit =
    syncOperation = Some(() => operation)
  
  /** Trigger a sync operation with automatic retry on failure.
    * Safe to call multiple times - will skip if already syncing.
    */
  def triggerSync(): Unit =
    if syncState.now() == SyncState.Syncing || awaitingStateReplace then
      println("Sync already in progress, skipping")
      return
    
    if !isConnectedVar.now() then
      println("WebSocket not connected, waiting for connection...")
      syncState.set(SyncState.Error("Waiting for connection"))
      return
    
    syncOperation match
      case Some(op) =>
        invalidateSyncRun()
        executeSyncWithRetry(op, 0, syncRunId)
      case None => println("No sync operation registered")
  
  private def executeSyncWithRetry(
    operation: () => Future[Unit],
    attempt: Int,
    runId: Long,
  ): Unit =
    if runId != syncRunId then
      return

    currentSyncAttempt = attempt
    val retryDelayMs = 2000 * math.pow(2, attempt).toInt // Exponential backoff: 2s, 4s, 8s
    clearSyncWaitState()
    syncState.set(SyncState.Syncing)
    
    println(s"Triggering state sync (attempt ${attempt + 1}/$maxSyncRetries)...")
    
    // Wrap the operation in a try-catch to handle any synchronous exceptions
    val futureResult = Try(operation()) match
      case Success(future) => future
      case Failure(ex) => Future.failed(ex)
    
    futureResult.onComplete {
      case Success(_) =>
        if runId != syncRunId then
          return
        // Verify still connected and still waiting on sync.
        // If a very fast StateReplace already arrived, syncState may already be Synced.
        if isConnectedVar.now() && syncState.now() == SyncState.Syncing then
          awaitingStateReplace = true
          syncAckTimeoutHandle = Some(
            dom.window.setTimeout(
              () =>
                if runId == syncRunId && awaitingStateReplace && syncState.now() == SyncState.Syncing then
                  handleSyncFailure("Timed out waiting for server snapshot", attempt, retryDelayMs, runId),
              syncAckTimeoutMs,
            ),
          )
        else if !isConnectedVar.now() then
          handleSyncFailure("Connection lost during sync", attempt, retryDelayMs, runId)
        else
          // Sync may already be marked as Synced by an early StateReplace.
          ()
          
      case Failure(ex) =>
        if runId == syncRunId then
          handleSyncFailure(ex.getMessage, attempt, retryDelayMs, runId)
    }
  
  private def handleSyncFailure(
    errorMsg: String,
    attempt: Int,
    retryDelayMs: Int,
    runId: Long,
  ): Unit =
    if runId != syncRunId then
      return

    println(s"Sync failed: $errorMsg")
    clearSyncWaitState()
    if attempt + 1 < maxSyncRetries then
      println(s"Retrying sync in ${retryDelayMs}ms...")
      syncState.set(SyncState.Error(s"Retrying... (${attempt + 1}/$maxSyncRetries)"))
      syncRetryTimeoutHandle = Some(dom.window.setTimeout(
        () =>
          if runId == syncRunId then
            syncOperation.foreach(op => executeSyncWithRetry(op, attempt + 1, runId)),
        retryDelayMs
      ))
    else
      println("Max sync retries reached, giving up")
      syncState.set(SyncState.Error(errorMsg))
      currentSyncAttempt = 0

  /** Mark sync as complete once a fresh StateReplace snapshot is received. */
  def markStateSynchronized(): Unit =
    if isConnectedVar.now() && syncState.now() != SyncState.Synced then
      clearSyncWaitState()
      syncState.set(SyncState.Synced)
      currentSyncAttempt = 0

  private def clearSyncWaitState(): Unit =
    awaitingStateReplace = false
    syncAckTimeoutHandle.foreach(dom.window.clearTimeout)
    syncAckTimeoutHandle = None
    syncRetryTimeoutHandle.foreach(dom.window.clearTimeout)
    syncRetryTimeoutHandle = None

  private def invalidateSyncRun(): Unit =
    syncRunId = syncRunId + 1
    clearSyncWaitState()
  
  // ============================================
  // Reconnect Operations
  // ============================================
  
  /** Force a full reconnection by toggling the WebSocket off and on.
    * This resets all retry counters and triggers a fresh sync.
    */
  def forceReconnect(): Unit =
    println("Force reconnecting WebSocket...")
    invalidateSyncRun()
    syncState.set(SyncState.Idle)
    reconnectAttemptVar.set(0)
    connectionEnabled.set(false)
    val _ = dom.window.setTimeout(() => connectionEnabled.set(true), 100)
  
  // ============================================
  // Event Handlers
  // ============================================
  
  private val onlineHandler: scalajs.js.Function1[dom.Event, Unit] = _ =>
    isOnlineVar.set(true)
    println("Network online - triggering reconnect")
    forceReconnect()
    
  private val offlineHandler: scalajs.js.Function1[dom.Event, Unit] = _ =>
    isOnlineVar.set(false)
  
  private val visibilityHandler: scalajs.js.Function1[dom.Event, Unit] = _ =>
    if dom.document.visibilityState == "hidden" then
      lastHiddenTime = Some(System.currentTimeMillis().toDouble)
      healthCheckHandle.foreach(dom.window.clearInterval(_))
      healthCheckHandle = None
    else if dom.document.visibilityState == "visible" then
      startHealthChecks()
      lastHiddenTime.foreach { hiddenTime =>
        val awayDuration = System.currentTimeMillis().toDouble - hiddenTime
        val awaySeconds = (awayDuration / 1000).toInt
        
        // Force reconnect if away longer than configured stale threshold.
        // This avoids aggressive reconnect churn on short focus changes.
        if awayDuration > staleThresholdMs then
          println(s"User returned after $awaySeconds seconds - forcing reconnect for reliability")
          forceReconnect()
      }
      lastHiddenTime = None
  
  private val pageshowHandler: scalajs.js.Function1[dom.PageTransitionEvent, Unit] = event =>
    if event.persisted then
      println("Page restored from bfcache - triggering reconnect")
      forceReconnect()
  
  private def startHealthChecks(): Unit =
    if healthCheckHandle.isEmpty then
      healthCheckHandle = Some(dom.window.setInterval(
        () => checkConnectionHealth(),
        healthCheckIntervalMs
      ))
  
  private def checkConnectionHealth(): Unit =
    val timeSinceLastMessage = System.currentTimeMillis().toDouble - lastMessageTime
    val connected = isConnectedVar.now()
    
    if connected && timeSinceLastMessage > 2 * 60 * 1000 then
      println(s"No messages received in ${(timeSinceLastMessage / 1000 / 60).toInt} minutes - connection may be stale")
      forceReconnect()
    else if !connected && reconnectAttemptVar.now() >= maxReconnectRetries then
      println("Retries exhausted but still disconnected - attempting reconnect")
      forceReconnect()

  // ============================================
  // Async Operation Wrapper
  // ============================================
  
  /** Execute an async operation with error handling.
    * Catches exceptions and reports them to userError.
    * Returns None if operation failed, Some(result) on success.
    */
  def withErrorHandling[T](
    operation: => Future[T],
    errorPrefix: String = "Operation failed",
  ): Future[Option[T]] =
    val futureResult = Try(operation) match
      case Success(future) => future
      case Failure(ex) => Future.failed(ex)
    
    futureResult.map(Some(_)).recover { case ex =>
      val message = s"$errorPrefix: ${ex.getMessage}"
      println(message)
      reportError(message)
      None
    }
  
  /** Execute an async operation with retry on failure.
    * Does not report to userError - use for background operations.
    */
  def withRetry[T](
    operation: => Future[T],
    maxRetries: Int = 3,
    onFailure: String => Unit = _ => (),
  ): Future[T] =
    def attempt(remaining: Int, lastError: Option[Throwable]): Future[T] =
      if remaining <= 0 then
        Future.failed(lastError.getOrElse(new Exception("Max retries exceeded")))
      else
        val futureResult = Try(operation) match
          case Success(future) => future
          case Failure(ex) => Future.failed(ex)
        
        futureResult.recoverWith { case ex =>
          val delayMs = 2000 * math.pow(2, maxRetries - remaining).toInt
          println(s"Operation failed, retrying in ${delayMs}ms: ${ex.getMessage}")
          onFailure(ex.getMessage)
          
          val promise = scala.concurrent.Promise[T]()
          val _ = dom.window.setTimeout(
            () => attempt(remaining - 1, Some(ex)).onComplete(promise.complete),
            delayMs
          )
          promise.future
        }
    
    attempt(maxRetries, None)

  // ============================================
  // Lifecycle Binders
  // ============================================
  
  /** Binder to attach event listeners to the DOM lifecycle */
  def bind[El <: ReactiveElement.Base]: Binder[El] =
    (element: El) =>
      ReactiveElement.bindSubscriptionUnsafe(element) { ctx =>
        dom.window.addEventListener("online", onlineHandler)
        dom.window.addEventListener("offline", offlineHandler)
        dom.document.addEventListener("visibilitychange", visibilityHandler)
        dom.window.addEventListener("pageshow", pageshowHandler)
        // Keep local connected tracking in lockstep with laminext's authoritative signal.
        ws.isConnected.changes.foreach(setConnected)(ctx.owner)
        setConnected(ws.isConnected.observe(ctx.owner).now())
        startHealthChecks()
        new com.raquo.airstream.ownership.Subscription(
          ctx.owner,
          cleanup = () => {
            dom.window.removeEventListener("online", onlineHandler)
            dom.window.removeEventListener("offline", offlineHandler)
            dom.document.removeEventListener("visibilitychange", visibilityHandler)
            dom.window.removeEventListener("pageshow", pageshowHandler)
            healthCheckHandle.foreach(dom.window.clearInterval(_))
            healthCheckHandle = None
          }
        )
      }

  /** Observer to handle WebSocket close events */
  val closeObserver: Observer[(dom.WebSocket, Boolean)] = Observer {
    case (_, willReconnect) =>
      setConnected(false)
      if willReconnect then
        reconnectAttemptVar.update(_ + 1)
      else
        reconnectAttemptVar.set(0)
  }

  /** Observer to handle successful connection */
  val connectedObserver: Observer[dom.WebSocket] = Observer { _ =>
    setConnected(true)
    reconnectAttemptVar.set(0)
  }

  // ============================================
  // Reactive Signals
  // ============================================
  
  /** Reactive signal representing the current connection state */
  val state: Signal[ConnectionState] =
    Signal.combine(
      ws.isConnected,
      ws.isConnecting,
      isOnlineVar.signal,
      reconnectAttemptVar.signal,
    ).map { case (isConnected, isConnecting, isOnline, attempts) =>
      if isConnected then
        ConnectionState.Connected
      else if isConnecting then
        ConnectionState.Connecting
      else if !isOnline then
        ConnectionState.Offline
      else if attempts > 0 && attempts <= maxReconnectRetries then
        ConnectionState.Reconnecting(attempts, maxReconnectRetries)
      else
        ConnectionState.Disconnected
    }

  /** Signal for whether to show the warning banner */
  val shouldShowBanner: Signal[Boolean] = state.map(_.shouldShowBanner)

  /** Signal for whether the connection is healthy */
  val isHealthy: Signal[Boolean] = state.map(_.isHealthy)
  
  /** Signal for whether the app is ready for user interactions.
    * True when connected AND sync is complete (not idle, syncing, or error).
    * Use this to block/disable interactions during reconnection.
    */
  val isReady: Signal[Boolean] = state.combineWith(syncState.signal).map {
    case (ConnectionState.Connected, SyncState.Synced) => true
    case _ => false
  }
  
  /** Check if app is ready for user interactions (synchronous check).
    * Use this in event handlers where you need an immediate boolean.
    */
  def checkReady(): Boolean =
    isConnectedVar.now() && syncState.now() == SyncState.Synced
  
  /** Combined sync message signal for UI */
  val syncMessage: Signal[String] = syncState.signal.map(_.message)

/** UI component for displaying connection status */
object ConnectionStatusIndicator:

  /** Small indicator dot for header/nav bar */
  def dot(state: Signal[ConnectionState]): HtmlElement =
    span(
      cls := "connection-indicator-dot",
      cls <-- state.map(_.cssClass),
      aria.label <-- state.map(s => s"Connection status: ${s.message}"),
      title <-- state.map(_.message),
      child.text <-- state.map(_.icon),
    )

  /** Full status display with icon and message */
  def full(state: Signal[ConnectionState]): HtmlElement =
    div(
      cls := "connection-indicator-full",
      cls <-- state.map(_.cssClass),
      aria.live := "polite",
      span(cls := "connection-icon", child.text <-- state.map(_.icon)),
      span(cls := "connection-message", child.text <-- state.map(_.message)),
    )

/** Banner component shown when connection is unhealthy or syncing */
object ConnectionStatusBanner:

  /** Renders a warning banner when disconnected/reconnecting */
  def apply(
    state: Signal[ConnectionState],
    onManualReconnect: Observer[Unit],
  ): HtmlElement =
    div(
      cls := "connection-banner",
      cls <-- state.map(_.cssClass),
      display <-- state.map(s => if s.shouldShowBanner then "flex" else "none"),
      aria.live := "assertive",
      role := "alert",
      div(
        cls := "connection-banner-content",
        span(cls := "connection-banner-icon", child.text <-- state.map(_.icon)),
        span(cls := "connection-banner-message", child.text <-- state.map(_.message)),
      ),
      child <-- state.map {
        case ConnectionState.Disconnected =>
          button(
            cls := "connection-banner-retry",
            onClick.mapToUnit --> onManualReconnect,
            "Retry",
          )
        case _ =>
          span()
      },
    )

  /** Renders a banner that shows both connection and sync status */
  def withSyncStatus(
    connectionState: Signal[ConnectionState],
    syncMessage: Signal[String],
    onManualReconnect: Observer[Unit],
  ): HtmlElement =
    val combinedMessage: Signal[String] = connectionState
      .combineWith(syncMessage)
      .map {
        case (ConnectionState.Connected, sync) if sync.nonEmpty => sync
        case (connState, _) => connState.message
      }
    
    val shouldShow: Signal[Boolean] = connectionState
      .combineWith(syncMessage)
      .map {
        case (connState, sync) =>
          connState.shouldShowBanner || sync.nonEmpty
      }
    
    div(
      cls := "connection-banner",
      cls <-- connectionState.map(_.cssClass),
      cls <-- connectionState.combineWith(syncMessage).map {
        case (ConnectionState.Connected, sync) if sync.nonEmpty => "connection-syncing"
        case _ => ""
      },
      display <-- shouldShow.map(show => if show then "flex" else "none"),
      aria.live := "assertive",
      role := "alert",
      div(
        cls := "connection-banner-content",
        span(cls := "connection-banner-icon", child.text <-- connectionState.map(_.icon)),
        span(cls := "connection-banner-message", child.text <-- combinedMessage),
      ),
      child <-- connectionState.map {
        case ConnectionState.Disconnected =>
          button(
            cls := "connection-banner-retry",
            onClick.mapToUnit --> onManualReconnect,
            "Retry",
          )
        case _ =>
          span()
      },
    )
