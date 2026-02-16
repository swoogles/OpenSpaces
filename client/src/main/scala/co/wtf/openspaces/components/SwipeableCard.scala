package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import org.scalajs.dom.window

import co.wtf.openspaces.{Discussion, DiscussionAction, Person, Feedback, VotePosition}
import co.wtf.openspaces.*
import io.laminext.websocket.*

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
  * 
  * Extracted from FrontEnd.scala for better code organization.
  */
object SwipeableCard:
  
  def apply(
    topic: Discussion,
    name: StrictSignal[Person],
    topicUpdates: DiscussionAction => Unit,
    cardContent: HtmlElement,
    connectionStatus: ConnectionStatusManager[DiscussionActionConfirmed, WebSocketMessage]
  ): HtmlElement =
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
