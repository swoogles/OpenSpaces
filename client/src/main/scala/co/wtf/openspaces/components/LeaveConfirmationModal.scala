package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import co.wtf.openspaces.{LeavableEntity, Person}
import neotype.unwrap

/** Reusable confirmation modal for leave/delete actions on any LeavableEntity.
  *
  * Shows appropriate messaging based on whether:
  * - The entity would be deleted (user is last person)
  * - Ownership would transfer (user is owner but others remain)
  * - User is just leaving (user is not owner)
  */
object LeaveConfirmationModal:

  /** Entity type names for display */
  enum EntityType(val displayName: String):
    case Topic extends EntityType("topic")
    case Project extends EntityType("project")
    case Activity extends EntityType("activity")

  /** Render a leave/delete confirmation modal.
    *
    * @param entity The LeavableEntity being left
    * @param entityType The type of entity for display purposes
    * @param entityTitle The title/name of the entity to display
    * @param currentUser The person leaving
    * @param onConfirm Called when user confirms the action
    * @param onCancel Called when user cancels
    * @return HtmlElement for the modal
    */
  def apply(
    entity: LeavableEntity,
    entityType: EntityType,
    entityTitle: String,
    currentUser: Person,
    onConfirm: () => Unit,
    onCancel: () => Unit,
  ): HtmlElement =
    val wouldDelete = entity.wouldBeDeletedIfLeaves(currentUser)
    val isOwner = entity.isOwner(currentUser)
    val nextOwnerName = entity.nextOwner.map(_.unwrap).getOrElse("someone else")
    val typeName = entityType.displayName

    val title =
      if wouldDelete then s"Delete '$entityTitle'?"
      else s"Leave '$entityTitle'?"

    val warningText =
      if wouldDelete then
        s"This $typeName will be deleted since no one else has joined."
      else if isOwner then
        s"You'll hand ownership to $nextOwnerName."
      else
        s"You'll leave this $typeName."

    val confirmText =
      if wouldDelete then s"Delete ${typeName.capitalize}"
      else s"Leave ${typeName.capitalize}"

    div(
      cls := "ConfirmationModal-overlay",
      onClick --> Observer(_ => onCancel()),
      div(
        cls := "ConfirmationModal",
        onClick.stopPropagation --> Observer.empty,
        h3(cls := "ConfirmationModal-title", title),
        p(cls := "ConfirmationModal-message", warningText),
        div(
          cls := "ConfirmationModal-buttons",
          button(
            cls := "ConfirmationModal-button ConfirmationModal-button--cancel",
            "Cancel",
            onClick --> Observer(_ => onCancel()),
          ),
          button(
            cls := "ConfirmationModal-button ConfirmationModal-button--confirm",
            confirmText,
            onClick --> Observer(_ => onConfirm()),
          ),
        ),
      ),
    )

  /** Empty placeholder when no modal should be shown */
  def empty: HtmlElement = div()
