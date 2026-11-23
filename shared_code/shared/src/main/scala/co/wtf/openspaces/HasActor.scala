package co.wtf.openspaces

/** Trait for confirmed actions that may have an associated actor (user who performed the action). */
trait HasActor:
  /** The person who performed this action, if applicable. */
  def actor: Option[Person]

  /** False for responses that don't change state (rejections, full snapshots),
    * which are not written to the confirmed-action log.
    */
  def isStateChange: Boolean

/** A client-to-server action. The actor is always known, so the server can
  * check it against the user the WebSocket connection authenticated as.
  */
trait ActorAction:
  def actor: Person
