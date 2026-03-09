package co.wtf.openspaces

/** Documents the Map-based state pattern used across entity types.
  *
  * All entity state classes in this codebase follow this pattern:
  * - Store entities in `Map[Id, Entity]`
  * - Provide an `apply(ActionConfirmed)` method for updates
  * - Return a new state instance (immutable)
  *
  * Implementations:
  * - DiscussionState: Map[TopicId, Discussion]
  * - LightningTalkState: Map[LightningTalkId, LightningTalkProposal]
  * - HackathonProjectState: Map[HackathonProjectId, HackathonProject]
  * - ActivityState: Map[ActivityId, Activity]
  * - LocationState: Map[Person, SharedLocation]
  *
  * This trait serves as documentation. Adding it as a mixin is optional
  * and may be done in the future if generic state handling is needed.
  */
trait EntityState[Id, Entity, ActionConfirmed]:
  /** The underlying entity map. */
  def entities: Map[Id, Entity]

  /** Apply a confirmed action to produce a new state. */
  def apply(action: ActionConfirmed): EntityState[Id, Entity, ActionConfirmed]
