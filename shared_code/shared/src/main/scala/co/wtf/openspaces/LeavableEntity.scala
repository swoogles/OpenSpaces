package co.wtf.openspaces

/** Trait for entities that support leave-with-ownership-transfer behavior.
  * 
  * When the owner leaves:
  * - If others are involved, ownership transfers to nextOwner
  * - If no one else is involved, the entity is deleted
  */
trait LeavableEntity:
  def isOwner(person: Person): Boolean
  def hasMember(person: Person): Boolean
  def nextOwner: Option[Person]
  
  /** Would this entity be deleted if the given person leaves? */
  def wouldBeDeletedIfLeaves(person: Person): Boolean =
    isOwner(person) && nextOwner.isEmpty
