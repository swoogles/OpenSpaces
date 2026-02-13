package co.wtf.openspaces

import zio.*

/** Common interface for discussion storage (in-memory or persistent) */
trait DiscussionStore:
  def snapshot: UIO[DiscussionState]
  def applyAction(discussionAction: DiscussionAction): Task[DiscussionActionConfirmed]
  def applyConfirmed(action: DiscussionActionConfirmed): UIO[Unit]
  def randomDiscussionAction: Task[DiscussionActionConfirmed]
  def randomScheduleAction: Task[DiscussionActionConfirmed]
