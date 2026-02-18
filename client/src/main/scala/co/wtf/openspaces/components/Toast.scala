package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

/** Toast notification manager for user alerts (e.g., "Your topic was scheduled") */
object ToastManager:
  case class Toast(id: Int, message: String, icon: String = "📍")
  
  private var nextId = 0
  val toasts: Var[List[Toast]] = Var(Nil)
  
  private val displayDurationMs = 5000
  private val maxToasts = 3
  
  def show(message: String, icon: String = "📍"): Unit =
    // DISABLED: Toast notifications are currently too noisy during chaos
    // Remove the return below to re-enable
    return
    
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
