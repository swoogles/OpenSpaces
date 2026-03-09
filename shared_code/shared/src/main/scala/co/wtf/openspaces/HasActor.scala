package co.wtf.openspaces

/** Trait for confirmed actions that may have an associated actor (user who performed the action). */
trait HasActor:
  /** The person who performed this action, if applicable. */
  def actor: Option[Person]
