package co.wtf.openspaces

import com.raquo.laminar.api.L.{*, given}
import neotype.unwrap

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

  // Current app view (Schedule or Topics)
  val currentAppView: Var[AppView] =
    Var(AppView.Topics)

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

  // ============================================
  // Leave Confirmation State
  // ============================================

  // Pending discussion leave confirmation
  val pendingLeaveDiscussion: Var[Option[Discussion]] = Var(None)
