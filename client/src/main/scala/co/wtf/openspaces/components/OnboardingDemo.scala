package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom.window

import co.wtf.openspaces.{AppState, VotePosition}

/** Interactive onboarding demo that shows users how to swipe-vote.
  * 
  * Displays two demo topic cards in sequence:
  * 1. First card auto-swipes right (Interested) after a delay
  * 2. Second card auto-swipes left (Not Interested) after the first settles
  * 3. Demo dismisses automatically, or user can skip with "Got it" button
  */
object OnboardingDemo:
  
  /** Phases of the onboarding demo */
  sealed trait DemoPhase
  object DemoPhase:
    case object ShowFirst extends DemoPhase
    case object SwipeFirst extends DemoPhase
    case object ShowSecond extends DemoPhase
    case object SwipeSecond extends DemoPhase
    case object Complete extends DemoPhase
  
  def apply(onComplete: () => Unit): HtmlElement =
    import DemoPhase.*
    
    val phase: Var[DemoPhase] = Var(ShowFirst)
    
    // Event buses for triggering auto-swipes
    val swipeFirstBus = new EventBus[AutoSwipeCommand]
    val swipeSecondBus = new EventBus[AutoSwipeCommand]
    
    // Demo topic card content
    def demoCard(title: String, emoji: String, description: String): HtmlElement =
      div(
        cls := "TopicCard TopicCard--demo",
        div(
          cls := "TopicCardHeader",
          span(cls := "TopicCardEmoji", emoji),
          span(cls := "TopicCardTitle", title),
        ),
        div(
          cls := "TopicCardDescription",
          description,
        ),
      )
    
    // Start the choreography on mount
    def startDemo(): Unit =
      // Phase 1: Show first card, wait, then swipe right
      val _ = window.setTimeout(() => {
        phase.set(SwipeFirst)
        swipeFirstBus.emit(AutoSwipeCommand(VotePosition.Interested, durationMs = 700))
      }, 1500)
    
    def onFirstSwipeComplete(): Unit =
      // Phase 2: Show second card after a pause
      val _ = window.setTimeout(() => {
        phase.set(ShowSecond)
        // Then swipe it left
        val _ = window.setTimeout(() => {
          phase.set(SwipeSecond)
          swipeSecondBus.emit(AutoSwipeCommand(VotePosition.NotInterested, durationMs = 700))
        }, 1200)
      }, 600)
    
    def onSecondSwipeComplete(): Unit =
      // Phase 3: Complete demo
      val _ = window.setTimeout(() => {
        phase.set(Complete)
        // Auto-dismiss after showing completion briefly
        val _ = window.setTimeout(() => {
          finishDemo()
        }, 800)
      }, 400)
    
    def finishDemo(): Unit =
      AppState.dismissSwipeHint()
      onComplete()
    
    div(
      cls := "OnboardingDemoOverlay",
      onMountCallback(_ => startDemo()),
      
      div(
        cls := "OnboardingDemoContent",
        
        // Header text
        div(
          cls := "OnboardingDemoHeader",
          h2("Swipe to Vote"),
          p("Swipe right if you're interested, left if you're not"),
        ),
        
        // Card container
        div(
          cls := "OnboardingDemoCards",
          
          // First demo card (swipe right)
          child <-- phase.signal.map {
            case ShowFirst | SwipeFirst =>
              SwipeableCard.demo(
                autoSwipe = swipeFirstBus.events,
                onSwipeComplete = () => onFirstSwipeComplete(),
                cardContent = demoCard(
                  "Exciting Topic",
                  "🚀",
                  "Something you'd love to discuss",
                ),
              )
            case _ => emptyNode
          },
          
          // Second demo card (swipe left)
          child <-- phase.signal.map {
            case ShowSecond | SwipeSecond =>
              SwipeableCard.demo(
                autoSwipe = swipeSecondBus.events,
                onSwipeComplete = () => onSecondSwipeComplete(),
                cardContent = demoCard(
                  "Boring Topic",
                  "😴",
                  "Not your thing? Swipe it away",
                ),
              )
            case _ => emptyNode
          },
          
          // Completion message
          child <-- phase.signal.map {
            case Complete =>
              div(
                cls := "OnboardingDemoComplete",
                span(cls := "OnboardingDemoCompleteIcon", "✓"),
                span("You're ready!"),
              )
            case _ => emptyNode
          },
        ),
        
        // Skip button
        button(
          cls := "OnboardingDemoSkip",
          "Got it",
          onClick --> { _ => finishDemo() },
        ),
        
        // Progress dots
        div(
          cls := "OnboardingDemoProgress",
          span(
            cls <-- phase.signal.map { p =>
              val active = p == ShowFirst || p == SwipeFirst
              if active then "OnboardingDemoDot OnboardingDemoDot--active" else "OnboardingDemoDot"
            },
          ),
          span(
            cls <-- phase.signal.map { p =>
              val active = p == ShowSecond || p == SwipeSecond
              if active then "OnboardingDemoDot OnboardingDemoDot--active" else "OnboardingDemoDot"
            },
          ),
          span(
            cls <-- phase.signal.map { p =>
              val active = p == Complete
              if active then "OnboardingDemoDot OnboardingDemoDot--active" else "OnboardingDemoDot"
            },
          ),
        ),
      ),
    )
