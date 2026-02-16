package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import co.wtf.openspaces.*
import io.laminext.websocket.*

/** Error banner that displays user-visible errors.
  * Merges local errors with connectionStatus.userError for unified error display.
  */
case class ErrorBanner(
  connectionStatus: ConnectionStatusManager[WebSocketMessage, WebSocketMessage],
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
