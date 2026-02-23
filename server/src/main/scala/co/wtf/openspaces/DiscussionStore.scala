package co.wtf.openspaces

import zio.*
import co.wtf.openspaces.discussions.{DiscussionAction, DiscussionActionConfirmed}
import co.wtf.openspaces.discussions.DiscussionState

/** Common interface for discussion storage (in-memory or persistent) */
trait DiscussionStore:
  def snapshot: UIO[DiscussionState]
  def applyAction(discussionAction: DiscussionAction): Task[DiscussionActionConfirmed]
  def applyConfirmed(action: DiscussionActionConfirmed): UIO[Unit]
  def randomDiscussionAction: Task[DiscussionActionConfirmed]
  def randomScheduleAction: Task[DiscussionActionConfirmed]
