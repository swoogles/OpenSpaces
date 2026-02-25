package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import org.scalajs.dom.window

enum SwipeDirection:
  case Left, Right

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

  /** Swipe direction if past threshold */
  def swipeDirection: Option[SwipeDirection] =
    if !isPastThreshold then None
    else if offsetX > 0 then Some(SwipeDirection.Right)
    else Some(SwipeDirection.Left)

/** Generic swipeable wrapper for cards.
  *
  * Configure optional left/right actions and handle typed actions in `onAction`.
  */
object SwipeableCard:

  case class Action[A](
    value: A,
    icon: String,
  )

  def apply[A](
    cardContent: HtmlElement,
    onAction: Observer[A],
    leftAction: Option[Action[A]] = None,
    rightAction: Option[Action[A]] = None,
  ): HtmlElement =
    val swipeState: Var[SwipeState] = Var(SwipeState())

    // Animation state for rubber-band effect
    val isAnimating: Var[Boolean] = Var(false)

    def isInteractiveTarget(target: dom.EventTarget | Null): Boolean =
      target match
        case null => false
        case node =>
          val elementOpt =
            node match
              case e: dom.Element => Some(e)
              case _ =>
                Option(node.asInstanceOf[dom.Node].parentNode).collect {
                  case e: dom.Element => e
                }
          elementOpt.exists { element =>
            element.matches("input, textarea, select, button, a, label, [contenteditable='true'], [data-no-swipe='true']") ||
            element.closest("input, textarea, select, button, a, label, [contenteditable='true'], [data-no-swipe='true']") != null
          }

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
        val maybeAction = state.swipeDirection.flatMap {
          case SwipeDirection.Left => leftAction
          case SwipeDirection.Right => rightAction
        }
        maybeAction.foreach(action => onAction.onNext(action.value))

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

    div(
      cls := "SwipeableCardContainer",
      // Left reveal (not interested - gray)
      div(
        cls := "SwipeReveal SwipeReveal--left",
        opacity <-- swipeState.signal.map { state =>
          if leftAction.isDefined && state.offsetX < 0 then math.abs(state.progress).min(1.0) else 0
        },
        // Icon scales up as you approach threshold
        div(
          cls := "SwipeRevealIcon",
          transform <-- swipeState.signal.map { state =>
            val scale = if leftAction.isDefined && state.offsetX < 0 then math.abs(state.progress).min(1.0) else 0
            s"scale($scale)"
          },
          leftAction.map(_.icon).getOrElse(""),
        ),
      ),
      // Right reveal (interested - green)
      div(
        cls := "SwipeReveal SwipeReveal--right",
        opacity <-- swipeState.signal.map { state =>
          if rightAction.isDefined && state.offsetX > 0 then state.progress.min(1.0) else 0
        },
        div(
          cls := "SwipeRevealIcon",
          transform <-- swipeState.signal.map { state =>
            val scale = if rightAction.isDefined && state.offsetX > 0 then state.progress.min(1.0) else 0
            s"scale($scale)"
          },
          rightAction.map(_.icon).getOrElse(""),
        ),
      ),
      // The actual card content (slides)
      div(
        cls := "SwipeableCardContent",
        cls <-- isAnimating.signal.map(if _ then "SwipeableCardContent--animating" else ""),
        transform <-- $transform,
        // Touch events
        onTouchStart --> { (e: dom.TouchEvent) =>
          val touch = e.touches(0)
          if !isInteractiveTarget(e.target) then
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
          if !isInteractiveTarget(e.target) then
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
          if leftAction.isDefined && state.offsetX < 0 && math.abs(state.progress) > 0.7 then
            (math.abs(state.progress) - 0.7) / 0.3
          else 0
        },
      ),
      div(
        cls := "SwipeThresholdIndicator SwipeThresholdIndicator--right",
        opacity <-- swipeState.signal.map { state =>
          if rightAction.isDefined && state.offsetX > 0 && state.progress > 0.7 then
            (state.progress - 0.7) / 0.3
          else 0
        },
      ),
    )
