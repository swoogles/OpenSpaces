package co.wtf.openspaces

import com.raquo.laminar.api.L.{*, given}
import neotype.unwrap

import org.scalajs.dom

import co.wtf.openspaces.discussions.DiscussionState
import co.wtf.openspaces.hackathon.*
import co.wtf.openspaces.services.AuthService
import co.wtf.openspaces.components.AppView
import co.wtf.openspaces.lighting_talks.LightningTalkState
import co.wtf.openspaces.activities.ActivityState
import co.wtf.openspaces.discussions.Discussion
import co.wtf.openspaces.discussions.VotePosition

/** Centralized app state (Vars + derived signals) */
object AppState:

  val discussionState: Var[DiscussionState] =
    Var(DiscussionState.empty)

  val lightningTalkState: Var[LightningTalkState] =
    Var(LightningTalkState.empty)

  val hackathonProjectState: Var[HackathonProjectState] =
    Var(HackathonProjectState.empty)

  val activityState: Var[ActivityState] =
    Var(ActivityState.empty)

  // ============================================
  // Vote Celebration (Sound + Visual Effects)
  // ============================================

  // Sound muted state - persisted to localStorage
  val soundMuted: Var[Boolean] = Var {
    SafeStorage.contains("soundMuted", "true")
  }

  // Persist sound muted state to localStorage
  soundMuted.signal.foreach { muted =>
    SafeStorage.setItem("soundMuted", muted.toString)
  }(unsafeWindowOwner)

  // Topics currently showing celebration animation (cleared after animation ends)
  val celebratingTopics: Var[Map[TopicId, VotePosition]] = Var(Map.empty)

  // Track which topic (if any) has its voter list expanded - ensures only one at a time
  val expandedVoterListTopicId: Var[Option[TopicId]] = Var(None)

  // ============================================
  // Swipe Hint (one-time onboarding)
  // ============================================

  // Whether user has seen the swipe hint (persisted)
  val hasSeenSwipeHint: Var[Boolean] = Var {
    SafeStorage.contains("hasSeenSwipeHint", "true")
  }

  // Whether to currently show the swipe hint UI
  val showSwipeHint: Var[Boolean] = Var {
    !SafeStorage.contains("hasSeenSwipeHint", "true")
  }

  /** Dismiss the swipe hint and remember it */
  def dismissSwipeHint(): Unit =
    showSwipeHint.set(false)
    hasSeenSwipeHint.set(true)
    SafeStorage.setItem("hasSeenSwipeHint", "true")

  val activeDiscussion: Var[Option[Discussion]] =
    Var(None)

  // Popover state
  val popoverState: Var[Option[Discussion]] =
    Var(None)

  // Swap action menu state: (selected Discussion, target Discussion)
  val swapMenuState: Var[Option[(Discussion, Discussion)]] =
    Var(None)

  // Unscheduled discussions menu state
  val unscheduledMenuState: Var[Option[RoomSlot]] =
    Var(None)

  // Active discussion actions menu state
  val activeDiscussionMenuState: Var[Option[Discussion]] =
    Var(None)

  // ============================================
  // URL Routing
  // ============================================

  /** Parse URL hash to AppView */
  def parseHashToView(hash: String): Option[AppView] =
    val normalized = hash.stripPrefix("#").stripPrefix("/").toLowerCase.trim
    normalized match
      case "topics" => Some(AppView.Topics)
      case "schedule" => Some(AppView.Schedule)
      case "activities" | "lightning" | "lightningtalks" => Some(AppView.LightningTalks)
      case "hackathon" | "hack" => Some(AppView.Hackathon)
      case "replay" | "tetris" => Some(AppView.Replay)
      case "admin" => Some(AppView.Admin)
      case "calendar" => Some(AppView.CalendarDayView)
      case _ => None

  /** Get the URL hash for a view */
  def viewToHash(view: AppView): String =
    view match
      case AppView.Topics => ""  // Default view, no hash
      case AppView.Schedule => "#schedule"
      case AppView.LightningTalks => "#activities"
      case AppView.Hackathon => "#hackathon"
      case AppView.Replay => "#replay"
      case AppView.Admin => "#admin"
      case AppView.CalendarDayView => "#calendar"

  /** Navigate to a view (updates URL hash and state) */
  def navigateTo(view: AppView): Unit =
    currentAppView.set(view)
    val hash = viewToHash(view)
    if hash.nonEmpty then
      dom.window.history.pushState(null, "", hash)
    else
      // Clear hash for default view
      dom.window.history.pushState(null, "", dom.window.location.pathname)

  /** Initialize routing from current URL hash */
  def initRouting(): Unit =
    // Set initial view from URL hash
    val initialHash = dom.window.location.hash
    parseHashToView(initialHash).foreach(currentAppView.set)

    // Listen for browser back/forward navigation
    dom.window.addEventListener("popstate", { (_: dom.PopStateEvent) =>
      parseHashToView(dom.window.location.hash).foreach(currentAppView.set)
    })

  // Current app view (Schedule or Topics)
  val currentAppView: Var[AppView] =
    Var(parseHashToView(dom.window.location.hash).getOrElse(AppView.Topics))

  // Name from auth cookie (immutable from GitHub)
  val name: Var[Person] = AuthService.getGitHubUsername()

  // Admin check - only admins can see admin controls
  val isAdmin: Signal[Boolean] = name.signal.map { person =>
    List("swoogles", "emma").exists(admin =>
      person.unwrap.toLowerCase().contains(admin)
    )
  }

  // Admin mode toggle - when false, admins see the normal user view
  val adminModeEnabled: Var[Boolean] = Var(
    SafeStorage.contains("adminModeEnabled", "true")
  )

  // Persist admin mode preference
  adminModeEnabled.signal.foreach { enabled =>
    SafeStorage.setItem("adminModeEnabled", enabled.toString)
  }(unsafeWindowOwner)

  // Loading screen preview toggle (admin only)
  val showLoadingPreview: Var[Boolean] = Var(false)

  // ============================================
  // Authorization State
  // ============================================

  // Whether the current user is approved to use the app
  val isAuthorized: Var[Boolean] = Var(false)

  // Whether the current user is a server-side admin (for user management)
  val isServerAdmin: Var[Boolean] = Var(false)

  // List of pending users (only populated for admins)
  val pendingUsers: Var[List[PendingUser]] = Var(List.empty)

  // List of approved users (only populated for admins)
  val approvedUsers: Var[List[ApprovedUser]] = Var(List.empty)

  // Whether we've loaded authorization status
  val authStatusLoaded: Var[Boolean] = Var(false)

  // Admin user management view state
  val showUserManagement: Var[Boolean] = Var(false)

  // Whether the current user has linked their Slack account
  val slackLinked: Var[Boolean] = Var(false)

  // ============================================
  // Slack Reply Counts
  // ============================================

  // Reply counts for Slack threads, keyed by entity ID
  val slackReplyCounts: Var[SlackReplyCounts] = Var(
    SlackReplyCounts(Map.empty, Map.empty, Map.empty, Map.empty)
  )

  // ============================================
  // Unjudged Counts (for nav badges)
  // ============================================

  // Derived signal: count of topics the current user hasn't voted on
  val unjudgedTopicCount: Signal[Int] = 
    discussionState.signal.combineWith(name.signal).map { case (state, currentUser) =>
      state.data.values.count(topic => 
        !topic.interestedParties.exists(_.voter == currentUser)
      )
    }

  // Derived signal: count of activities the current user hasn't voted on
  val unjudgedActivityCount: Signal[Int] =
    activityState.signal.combineWith(name.signal).map { case (state, currentUser) =>
      state.activities.values.count(activity => !activity.hasMember(currentUser))
    }

  // ============================================
  // Unified Entity Creation (bottom sheet)
  // ============================================

  /** Entity types that can be created via the unified bottom sheet */
  enum CreateEntityType:
    case Topic
    case Activity
    case LightningTalk
    case HackathonProject

  /** Whether the create sheet is open */
  val createSheetOpen: Var[Boolean] = Var(false)

  /** Open the create sheet */
  def openCreateSheet(): Unit =
    createSheetOpen.set(true)

  /** Close the create sheet */
  def closeCreateSheet(): Unit =
    createSheetOpen.set(false)

  // Legacy alias for backwards compatibility during transition
  @deprecated("Use createSheetState instead", "")
  val showCreateActivityForm: Var[Boolean] = Var(false)

  // ID of newly created activity to scroll to (set after creation, cleared after scroll)
  val scrollToActivityId: Var[Option[String]] = Var(None)

  // ============================================
  // Location Sharing
  // ============================================

  import co.wtf.openspaces.location.{LocationState, SharedLocation}

  // Server-side state: who's currently sharing
  val locationState: Var[LocationState] = Var(LocationState.empty)

  // Am I currently sharing? (local toggle)
  val locationSharingEnabled: Var[Boolean] = Var(false)

  // When my sharing expires (for countdown display)
  val locationExpiresAt: Var[Option[Long]] = Var(None)

  // Derived signal: count of people sharing
  val sharersCount: Signal[Int] = locationState.signal.map(_.sharingCount)

  // Derived signal: get my location if I'm sharing
  def myLocation: Signal[Option[SharedLocation]] =
    locationState.signal.combineWith(name.signal).map { case (state, person) =>
      state.locations.get(person)
    }

  // Update location state from server
  def updateLocationState(newState: LocationState): Unit =
    locationState.set(newState)

  // When we start sharing, track expiration
  def setLocationSharing(enabled: Boolean, expiresAt: Option[Long] = None): Unit =
    locationSharingEnabled.set(enabled)
    locationExpiresAt.set(expiresAt)
