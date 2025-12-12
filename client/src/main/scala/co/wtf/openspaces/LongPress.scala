package co.wtf.openspaces

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

/** Provides iOS-compatible long press detection using touch events.
  *
  * iOS Safari doesn't fire contextmenu events on long press like Android and
  * desktop browsers do. This utility implements long press detection using
  * touch events with a timer.
  *
  * Usage:
  * {{{
  * div(
  *   LongPress.binder { () =>
  *     // Long press action here
  *     println("Long pressed!")
  *   }
  * )
  * }}}
  */
object LongPress:

  /** Duration in milliseconds to wait before triggering long press */
  private val LongPressDuration: Int = 500

  /** Maximum movement in pixels allowed during long press before canceling */
  private val MovementThreshold: Double = 10.0

  /** Creates a binder that handles long press via touch events (iOS/mobile) and
    * contextmenu (desktop/Android).
    *
    * @param onLongPress
    *   Callback to execute when long press is detected
    * @return
    *   A modifier that can be applied to HTML elements
    */
  def binder(
    onLongPress: () => Unit,
  ): Modifier[HtmlElement] =
    val longPressState = new LongPressState()

    Seq(
      // Touch events for iOS/mobile
      onTouchStart --> { (event: dom.TouchEvent) =>
        event.preventDefault()
        longPressState.startTouch(event, onLongPress)
      },
      onTouchMove --> { (event: dom.TouchEvent) =>
        longPressState.handleMove(event)
      },
      onTouchEnd --> { (_: dom.TouchEvent) =>
        longPressState.cancel()
      },
      onTouchCancel --> { (_: dom.TouchEvent) =>
        longPressState.cancel()
      },
      // Context menu for desktop right-click and Android long press
      onContextMenu.preventDefault --> { (event: dom.MouseEvent) =>
        event.stopPropagation()
        onLongPress()
      },
    )

  /** Creates a binder that handles long press but doesn't prevent default on
    * touchstart. This is useful when you want to allow the element to still
    * receive click events normally.
    *
    * @param onLongPress
    *   Callback to execute when long press is detected
    * @return
    *   A modifier that can be applied to HTML elements
    */
  def binderAllowClicks(
    onLongPress: () => Unit,
  ): Modifier[HtmlElement] =
    val longPressState = new LongPressState()

    Seq(
      // Touch events for iOS/mobile - don't preventDefault to allow clicks
      onTouchStart --> { (event: dom.TouchEvent) =>
        longPressState.startTouch(event, onLongPress)
      },
      onTouchMove --> { (event: dom.TouchEvent) =>
        longPressState.handleMove(event)
      },
      onTouchEnd --> { (_: dom.TouchEvent) =>
        longPressState.cancel()
      },
      onTouchCancel --> { (_: dom.TouchEvent) =>
        longPressState.cancel()
      },
      // Context menu for desktop right-click and Android long press
      onContextMenu.preventDefault --> { (event: dom.MouseEvent) =>
        event.stopPropagation()
        onLongPress()
      },
    )

  /** Internal class to manage long press state and timer */
  private class LongPressState:
    private var timerId: Option[Int] = None
    private var startX: Double = 0
    private var startY: Double = 0
    private var triggered: Boolean = false

    def startTouch(
      event: dom.TouchEvent,
      onLongPress: () => Unit,
    ): Unit =
      cancel()
      triggered = false

      val touch = event.touches(0)
      startX = touch.clientX
      startY = touch.clientY

      val id = dom.window.setTimeout(
        () => {
          triggered = true
          onLongPress()
        },
        LongPressDuration,
      )
      timerId = Some(id)

    def handleMove(
      event: dom.TouchEvent,
    ): Unit =
      if (timerId.isDefined && event.touches.length > 0) {
        val touch = event.touches(0)
        val dx = touch.clientX - startX
        val dy = touch.clientY - startY
        val distance = Math.sqrt(dx * dx + dy * dy)

        if (distance > MovementThreshold) {
          cancel()
        }
      }

    def cancel(): Unit =
      timerId.foreach(dom.window.clearTimeout)
      timerId = None

    def wasTriggered: Boolean = triggered
