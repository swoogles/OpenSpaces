package co.wtf.openspaces.util

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import org.scalajs.dom.window

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
