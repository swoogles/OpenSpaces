package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom.window

import co.wtf.openspaces.{AppState, TopicId, VotePosition}

/** Orchestrates auto-swipe onboarding for new users.
  * 
  * When a new user logs in (hasSeenSwipeHint = false), this triggers
  * auto-swipe animations on the first two unjudged topics in sequence:
  * 1. First topic swipes right (Interested) after initial delay
  * 2. Second topic swipes left (Not Interested) after first completes
  * 3. Marks onboarding complete so it doesn't repeat
  * 
  * The auto-swipes send real votes to the server, so these are actual
  * votes on real topics (which should be seeded appropriately).
  */
object OnboardingOrchestrator:
  
  /** Current phase of onboarding */
  sealed trait Phase
  object Phase:
    case object Waiting extends Phase       // Waiting for topics to load
    case object SwipingFirst extends Phase  // Auto-swiping first topic
    case object SwipingSecond extends Phase // Auto-swiping second topic
    case object Complete extends Phase      // Onboarding finished
  
  // Current phase
  private val phase: Var[Phase] = Var(Phase.Waiting)
  
  // Event buses for triggering auto-swipes on specific topics
  private val swipeBuses: Var[Map[TopicId, EventBus[AutoSwipeCommand]]] = Var(Map.empty)
  
  // Topics queued for onboarding (first two unjudged)
  private val onboardingTopics: Var[List[TopicId]] = Var(Nil)
  
  /** Check if onboarding is active */
  def isActive: Signal[Boolean] = phase.signal.map(_ != Phase.Complete)
  
  /** Get the auto-swipe stream for a specific topic (if it's in the onboarding queue) */
  def autoSwipeFor(topicId: TopicId): Option[(EventStream[AutoSwipeCommand], () => Unit)] =
    val buses = swipeBuses.now()
    buses.get(topicId).map { bus =>
      (bus.events, () => onSwipeComplete(topicId))
    }
  
  /** Register topics for onboarding (called when topic list first renders) */
  def registerTopics(unjudgedTopicIds: List[TopicId]): Unit =
    // Only proceed if we haven't seen the hint
    if !AppState.showSwipeHint.now() then return
    if phase.now() != Phase.Waiting then return
    
    // Take first two unjudged topics
    val toOnboard = unjudgedTopicIds.take(2)
    if toOnboard.isEmpty then return
    
    // Create event buses for each
    val buses = toOnboard.map(id => id -> new EventBus[AutoSwipeCommand]).toMap
    swipeBuses.set(buses)
    onboardingTopics.set(toOnboard)
    
    // Start onboarding after a delay
    val _ = window.setTimeout(() => startOnboarding(), 2000)
  
  /** Start the onboarding sequence */
  private def startOnboarding(): Unit =
    val topics = onboardingTopics.now()
    val buses = swipeBuses.now()
    
    topics.headOption.foreach { firstId =>
      buses.get(firstId).foreach { bus =>
        phase.set(Phase.SwipingFirst)
        // Swipe right (Interested)
        bus.emit(AutoSwipeCommand(VotePosition.Interested, durationMs = 800))
      }
    }
  
  /** Called when a swipe animation completes */
  private def onSwipeComplete(topicId: TopicId): Unit =
    val topics = onboardingTopics.now()
    val buses = swipeBuses.now()
    
    phase.now() match
      case Phase.SwipingFirst =>
        // First swipe done, trigger second after a pause
        topics.lift(1) match
          case Some(secondId) =>
            val _ = window.setTimeout(() => {
              buses.get(secondId).foreach { bus =>
                phase.set(Phase.SwipingSecond)
                // Swipe left (Not Interested)
                bus.emit(AutoSwipeCommand(VotePosition.NotInterested, durationMs = 800))
              }
            }, 1000)
          case None =>
            // Only one topic, finish
            finishOnboarding()
      
      case Phase.SwipingSecond =>
        // Second swipe done, finish onboarding
        finishOnboarding()
      
      case _ => ()
  
  /** Complete the onboarding process */
  private def finishOnboarding(): Unit =
    phase.set(Phase.Complete)
    AppState.dismissSwipeHint()
    // Clear state
    swipeBuses.set(Map.empty)
    onboardingTopics.set(Nil)
  
  /** Reset orchestrator (for testing/admin) */
  def reset(): Unit =
    phase.set(Phase.Waiting)
    swipeBuses.set(Map.empty)
    onboardingTopics.set(Nil)
