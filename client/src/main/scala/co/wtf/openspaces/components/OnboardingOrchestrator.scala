package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom.window

import co.wtf.openspaces.{AppState, Person, TopicId, VotePosition}

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
  
  // Global event bus for auto-swipe commands (topicId, command)
  private val swipeBus: EventBus[(TopicId, AutoSwipeCommand)] = new EventBus
  
  // Topics queued for onboarding (first two unjudged)
  private val onboardingTopics: Var[List[TopicId]] = Var(Nil)
  
  // Track which topics have completed their swipe
  private val completedTopics: Var[Set[TopicId]] = Var(Set.empty)
  
  /** Check if onboarding is active */
  def isActive: Signal[Boolean] = phase.signal.map(_ != Phase.Complete)
  
  /** Get the auto-swipe stream for a specific topic.
    * Returns a filtered stream + completion callback.
    * Cards call this during render - the stream is always available.
    */
  def autoSwipeFor(topicId: TopicId): (EventStream[AutoSwipeCommand], () => Unit) =
    val filteredStream = swipeBus.events.collect {
      case (id, cmd) if id == topicId => cmd
    }
    (filteredStream, () => onSwipeComplete(topicId))
  
  /** Register for onboarding (called when topic list first renders for a new user) */
  def registerTopics(currentUser: Person): Unit =
    // Only proceed if we haven't seen the hint
    if !AppState.showSwipeHint.now() then return
    if phase.now() != Phase.Waiting then return
    
    // Get ALL unjudged topics from the full state (not the filtered view)
    val allTopics = AppState.discussionState.now().data.values.toList
    val unjudgedTopicIds = allTopics
      .filterNot(_.interestedParties.exists(_.voter == currentUser))
      .map(_.id)
    
    // Take first two unjudged topics
    val toOnboard = unjudgedTopicIds.take(2)
    if toOnboard.isEmpty then return
    
    println(s"[Onboarding] Registering ${toOnboard.length} topics for onboarding: $toOnboard")
    
    onboardingTopics.set(toOnboard)
    completedTopics.set(Set.empty)
    
    // Start onboarding after a delay
    val _ = window.setTimeout(() => startOnboarding(), 2000)
  
  /** Start the onboarding sequence */
  private def startOnboarding(): Unit =
    val topics = onboardingTopics.now()
    println(s"[Onboarding] Starting onboarding with topics: $topics, phase: ${phase.now()}")
    
    topics.headOption.foreach { firstId =>
      phase.set(Phase.SwipingFirst)
      println(s"[Onboarding] Emitting swipe right for topic $firstId")
      // Swipe right (Interested)
      swipeBus.emit((firstId, AutoSwipeCommand(VotePosition.Interested, durationMs = 800)))
    }
  
  /** Called when a swipe animation completes */
  private def onSwipeComplete(topicId: TopicId): Unit =
    completedTopics.update(_ + topicId)
    val topics = onboardingTopics.now()
    
    phase.now() match
      case Phase.SwipingFirst =>
        // First swipe done, trigger second after a pause
        topics.lift(1) match
          case Some(secondId) =>
            val _ = window.setTimeout(() => {
              phase.set(Phase.SwipingSecond)
              // Swipe left (Not Interested)
              swipeBus.emit((secondId, AutoSwipeCommand(VotePosition.NotInterested, durationMs = 800)))
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
    onboardingTopics.set(Nil)
    completedTopics.set(Set.empty)
  
  /** Reset orchestrator (for testing/admin) */
  def reset(): Unit =
    phase.set(Phase.Waiting)
    onboardingTopics.set(Nil)
    completedTopics.set(Set.empty)
