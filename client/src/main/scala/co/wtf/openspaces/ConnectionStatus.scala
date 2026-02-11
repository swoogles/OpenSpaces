package co.wtf.openspaces

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveElement
import io.laminext.websocket.WebSocket
import org.scalajs.dom

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

/** Manages WebSocket connection state and provides reactive signals for UI.
  *
  * @param ws
  *   The laminext WebSocket instance to monitor
  * @param maxReconnectRetries
  *   Maximum number of reconnection attempts (should match WebSocket config)
  * @param staleThresholdMs
  *   Time in milliseconds after which a return to the page triggers reconnection (default: 1 hour)
  */
class ConnectionStatusManager[Receive, Send](
  ws: WebSocket[Receive, Send],
  maxReconnectRetries: Int = 10,
  staleThresholdMs: Double = 60 * 60 * 1000, // 1 hour default
):
  // Track reconnection attempts
  private val reconnectAttemptVar: Var[Int] = Var(0)

  // Track if device is online
  private val isOnlineVar: Var[Boolean] = Var(dom.window.navigator.onLine)

  // Track when page was last hidden (for stale detection)
  private var lastHiddenTime: Option[Double] = None
  
  // Callback for when user returns after being away too long
  private var onStaleReturnCallback: Option[() => Unit] = None
  
  /** Set a callback to be invoked when user returns after stale threshold */
  def onStaleReturn(callback: => Unit): Unit =
    onStaleReturnCallback = Some(() => callback)

  // Set up online/offline event listeners
  private val onlineHandler: scalajs.js.Function1[dom.Event, Unit] = _ => isOnlineVar.set(true)
  private val offlineHandler: scalajs.js.Function1[dom.Event, Unit] = _ => isOnlineVar.set(false)
  
  // Visibility change handler - detect stale returns
  private val visibilityHandler: scalajs.js.Function1[dom.Event, Unit] = _ =>
    if dom.document.visibilityState == "hidden" then
      lastHiddenTime = Some(System.currentTimeMillis().toDouble)
    else if dom.document.visibilityState == "visible" then
      lastHiddenTime.foreach { hiddenTime =>
        val awayDuration = System.currentTimeMillis().toDouble - hiddenTime
        if awayDuration > staleThresholdMs then
          println(s"User returned after ${(awayDuration / 1000 / 60).toInt} minutes - triggering reconnect")
          onStaleReturnCallback.foreach(_())
      }
      lastHiddenTime = None
  
  // Handle bfcache restoration (page was frozen/restored)
  private val pageshowHandler: scalajs.js.Function1[dom.PageTransitionEvent, Unit] = event =>
    if event.persisted then
      println("Page restored from bfcache - triggering reconnect")
      onStaleReturnCallback.foreach(_())

  /** Binder to attach online/offline/visibility listeners to the DOM lifecycle */
  def bind[El <: ReactiveElement.Base]: Binder[El] =
    (element: El) =>
      ReactiveElement.bindSubscriptionUnsafe(element) { ctx =>
        dom.window.addEventListener("online", onlineHandler)
        dom.window.addEventListener("offline", offlineHandler)
        dom.document.addEventListener("visibilitychange", visibilityHandler)
        dom.window.addEventListener("pageshow", pageshowHandler)
        new com.raquo.airstream.ownership.Subscription(
          ctx.owner,
          cleanup = () => {
            dom.window.removeEventListener("online", onlineHandler)
            dom.window.removeEventListener("offline", offlineHandler)
            dom.document.removeEventListener("visibilitychange", visibilityHandler)
            dom.window.removeEventListener("pageshow", pageshowHandler)
          }
        )
      }

  /** Observer to handle WebSocket close events and track reconnection attempts */
  val closeObserver: Observer[(dom.WebSocket, Boolean)] = Observer {
    case (_, willReconnect) =>
      if willReconnect then
        reconnectAttemptVar.update(_ + 1)
      else
        reconnectAttemptVar.set(0)
  }

  /** Observer to handle successful connection (resets reconnect counter) */
  val connectedObserver: Observer[dom.WebSocket] = Observer { _ =>
    reconnectAttemptVar.set(0)
  }

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

/** UI component for displaying connection status */
object ConnectionStatusIndicator:

  /** Small indicator dot for header/nav bar
    *
    * @param state
    *   Reactive signal of connection state
    */
  def dot(state: Signal[ConnectionState]): HtmlElement =
    span(
      cls := "connection-indicator-dot",
      cls <-- state.map(_.cssClass),
      aria.label <-- state.map(s => s"Connection status: ${s.message}"),
      title <-- state.map(_.message),
      child.text <-- state.map(_.icon),
    )

  /** Full status display with icon and message
    *
    * @param state
    *   Reactive signal of connection state
    */
  def full(state: Signal[ConnectionState]): HtmlElement =
    div(
      cls := "connection-indicator-full",
      cls <-- state.map(_.cssClass),
      aria.live := "polite",  // Announce changes to screen readers
      span(cls := "connection-icon", child.text <-- state.map(_.icon)),
      span(cls := "connection-message", child.text <-- state.map(_.message)),
    )

/** Banner component shown when connection is unhealthy or syncing */
object ConnectionStatusBanner:

  /** Renders a warning banner when disconnected/reconnecting
    *
    * @param state
    *   Reactive signal of connection state
    * @param onManualReconnect
    *   Observer to trigger manual reconnection attempt
    */
  def apply(
    state: Signal[ConnectionState],
    onManualReconnect: Observer[Unit],
  ): HtmlElement =
    div(
      cls := "connection-banner",
      cls <-- state.map(_.cssClass),
      display <-- state.map(s => if s.shouldShowBanner then "flex" else "none"),
      aria.live := "assertive",  // Immediately announce to screen readers
      role := "alert",
      div(
        cls := "connection-banner-content",
        span(cls := "connection-banner-icon", child.text <-- state.map(_.icon)),
        span(cls := "connection-banner-message", child.text <-- state.map(_.message)),
      ),
      // Show retry button only when disconnected (not auto-reconnecting)
      child <-- state.map {
        case ConnectionState.Disconnected =>
          button(
            cls := "connection-banner-retry",
            onClick.mapToUnit --> onManualReconnect,
            "Retry",
          )
        case _ =>
          span()  // Empty placeholder
      },
    )

  /** Renders a banner that shows both connection and sync status.
    * Shows sync message when connected but syncing data.
    *
    * @param connectionState
    *   Reactive signal of connection state
    * @param syncMessage
    *   Reactive signal of sync status message (empty when not syncing)
    * @param onManualReconnect
    *   Observer to trigger manual reconnection attempt
    */
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
      // Also add syncing class when connected but syncing
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
