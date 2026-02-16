package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import neotype.*
import co.wtf.openspaces.{Discussion, DiscussionAction, Person, RoomSlot, Topic, GitHubAvatar, GlyphiconUtils, SvgIcon}
import co.wtf.openspaces.util.MenuPositioning

/** Swap action menu for swapping two scheduled topics */
object Menu:
  def apply(
    selectedDiscussion: Discussion,
    targetDiscussion: Discussion,
    topicUpdates: DiscussionAction => Unit,
    dismissMenu: Observer[Unit],
  ) =
    val (x, y) = MenuPositioning.standardMenuPosition()
    div(
      cls := "Menu",
      left := s"${x}px",
      top := s"${y}px",
      onClick.preventDefault.stopPropagation --> Observer(_ => ()),
      div(cls := "Menu-header", "Actions"),
      // Selected topic (current selection)
      div(
        cls := "Menu-section",
        div(cls := "Menu-label", "Selected Topic:"),
        div(
          cls := "Menu-topic Menu-topic--selected",
          GitHubAvatar(selectedDiscussion.facilitator),
          div(
            div(cls := "Menu-topicName", selectedDiscussion.topicName),
            div(
              cls := "Menu-roomSlot",
              selectedDiscussion.roomSlot
                .map(_.displayString)
                .getOrElse("Unscheduled"),
            ),
          ),
        ),
      ),
      // Target topic
      div(
        cls := "Menu-section Menu-section--target",
        div(cls := "Menu-label", "Target Topic:"),
        div(
          cls := "Menu-topic Menu-topic--target",
          GitHubAvatar(targetDiscussion.facilitator),
          div(
            div(cls := "Menu-topicName", targetDiscussion.topicName),
            div(
              cls := "Menu-roomSlot",
              targetDiscussion.roomSlot
                .map(_.displayString)
                .getOrElse("Unscheduled"),
            ),
          ),
        ),
      ),
      // Action buttons
      div(
        cls := "Menu-actions",
        button(
          cls := "Menu-swapButton",
          onClick --> Observer { _ =>
            // Both discussions must have room slots for swap to work
            (selectedDiscussion.roomSlot,
             targetDiscussion.roomSlot,
            ) match
              case (Some(slot1), Some(slot2)) =>
                topicUpdates(
                  DiscussionAction.SwapTopics(
                    selectedDiscussion.id,
                    slot1,
                    targetDiscussion.id,
                    slot2,
                  ),
                )
                dismissMenu.onNext(())
              case _ => () // Should not happen - UI prevents this
          },
          span("⇅"),
          span("Swap Room Slots"),
        ),
        button(
          cls := "Menu-cancelButton",
          onClick.mapToUnit --> dismissMenu,
          "Cancel",
        ),
      ),
    )

/** Menu for selecting unscheduled discussions to assign to a room slot */
object UnscheduledDiscussionsMenu:
  def apply(
    unscheduledDiscussions: List[Discussion],
    targetRoomSlot: RoomSlot,
    facilitator: StrictSignal[Person],
    topicUpdates: DiscussionAction => Unit,
    dismissMenu: Observer[Unit],
    setActiveDiscussion: Observer[Discussion],
    activeDiscussion: Option[Discussion],
    currentView: AppView,
  ) =
    val (x, y) = MenuPositioning.standardMenuPosition()
    val textVar = Var("")
    val errorVar = Var[Option[String]](None)

    def submitNewDiscussion() =
      val topicAttempt =
        Topic.make(textVar.now())

      topicAttempt match
        case Left(error) =>
          errorVar.set(Some(error))
        case Right(topic) =>
          val facilitatorName = facilitator.now()
          if (facilitatorName.unwrap.trim.length < 2) {
            errorVar.set(
              Some(
                "Please enter your name (2+ characters) before adding.",
              ),
            )
          }
          else {
            topicUpdates(
              DiscussionAction.AddWithRoomSlot(
                topic,
                facilitatorName,
                targetRoomSlot,
              ),
            )
            textVar.set("")
            errorVar.set(None)
            dismissMenu.onNext(())
          }

    // Determine if we should show the "Move current topic here" option
    // Only show if:
    // 1. We're on the Admin view (where the selected topic is visible)
    // 2. There's an active discussion that could be moved to this slot
    val moveCurrentTopicOption: Option[HtmlElement] = 
      if (currentView != AppView.Admin) None
      else activeDiscussion.flatMap { discussion =>
        // Only show if the active discussion is not already in this slot
        if (discussion.roomSlot.contains(targetRoomSlot)) None
        else
          Some(
            div(
              cls := "Menu-section",
              div(cls := "Menu-label", "Move Current Topic Here:"),
              div(
                cls := "Menu-actions",
                button(
                  cls := "Menu-swapButton",
                  onClick --> Observer { _ =>
                    topicUpdates(
                      DiscussionAction.SetRoomSlot(
                        discussion.id,
                        discussion.roomSlot,
                        Some(targetRoomSlot),
                      ),
                    )
                    dismissMenu.onNext(())
                  },
                  GitHubAvatar(discussion.facilitator),
                  span(
                    cls := "Menu-topicName",
                    discussion.topicName,
                  ),
                ),
              ),
            ),
          )
    }

    div(
      cls := "Menu",
      left := s"${x}px",
      top := s"${y}px",
      onClick.preventDefault.stopPropagation --> Observer(_ => ()),
      div(cls := "Menu-header", "Assign Discussion"),
      div(
        cls := "Menu-section",
        div(cls := "Menu-label",
            s"Room Slot: ${targetRoomSlot.displayString}",
        ),
      ),
      // Move current topic option (if applicable)
      moveCurrentTopicOption.getOrElse(span()),
      div(
        cls := "Menu-section",
        div(cls := "Menu-label", "Create New Discussion:"),
        textArea(
          cls := "Menu-textArea",
          placeholder := "Describe the discussion to schedule...",
          value <-- textVar.signal,
          onInput.mapToValue --> textVar,
          onMountCallback(ctx => ctx.thisNode.ref.focus()),
        ),
        button(
          cls := "Menu-swapButton",
          onClick --> Observer { _ =>
            submitNewDiscussion()
          },
          "Add & Assign",
        ),
        child <--
          errorVar.signal.map {
            case Some(errorMessage) =>
              div(cls := "Menu-error", errorMessage)
            case None =>
              span()
          },
      ),
      div(
        cls := "Menu-section",
        div(cls := "Menu-label", "Move Existing Unscheduled Topic:"),
        if (unscheduledDiscussions.isEmpty) {
          div(
            cls := "Menu-topic",
            div(
              div(cls := "Menu-topicName",
                  "No unscheduled discussions available",
              ),
            ),
          )
        }
        else {
          div(
            cls := "Menu-actions",
            unscheduledDiscussions.map { discussion =>
              button(
                cls := "Menu-swapButton",
                onClick --> Observer { _ =>
                  topicUpdates(
                    DiscussionAction.SetRoomSlot(
                      discussion.id,
                      discussion.roomSlot,
                      Some(targetRoomSlot),
                    ),
                  )
                  // Set the discussion as active after assigning it to the room slot
                  setActiveDiscussion.onNext(
                    discussion.copy(roomSlot = Some(targetRoomSlot)),
                  )
                  dismissMenu.onNext(())
                },
                GitHubAvatar(discussion.facilitator),
                span(
                  cls := "Menu-topicName",
                  discussion.topicName,
                ),
              )
            },
          )
        },
      ),
      div(
        cls := "Menu-actions",
        button(
          cls := "Menu-cancelButton",
          onClick.mapToUnit --> dismissMenu,
          "Cancel",
        ),
      ),
    )

/** Menu for actions on the currently active/selected discussion */
object ActiveDiscussionActionMenu:
  def apply(
    discussion: Discussion,
    topicUpdates: DiscussionAction => Unit,
    dismissMenu: Observer[Unit],
  ) =
    val (x, y) = MenuPositioning.standardMenuPosition()
    val isScheduled = discussion.roomSlot.isDefined

    val cancelButton =
      button(
        cls := "Menu-cancelButton",
        onClick.mapToUnit --> dismissMenu,
        "Close",
      )

    val actionElements: List[Modifier[HtmlElement]] =
      if isScheduled then
        List(
          button(
            cls := "Menu-swapButton",
            onClick --> Observer { _ =>
              topicUpdates(
                DiscussionAction.SetRoomSlot(
                  discussion.id,
                  discussion.roomSlot,
                  None,
                ),
              )
              dismissMenu.onNext(())
            },
            SvgIcon(GlyphiconUtils.minus),
            span("Unschedule topic"),
          ),
          cancelButton,
        )
      else
        List(
          div(
            cls := "Menu-topic Menu-topic--selected",
            span(
              cls := "Menu-topicName",
              "This discussion is not scheduled yet.",
            ),
          ),
          cancelButton,
        )

    val actionSection =
      (cls := "Menu-actions") :: actionElements

    div(
      cls := "Menu",
      left := s"${x}px",
      top := s"${y}px",
      onClick.preventDefault.stopPropagation --> Observer(_ => ()),
      div(cls := "Menu-header", "Discussion actions"),
      div(
        cls := "Menu-topic Menu-topic--selected",
        GitHubAvatar(discussion.facilitator),
        div(
          div(
            cls := "Menu-topicName",
            discussion.topicName,
          ),
          div(
            cls := "Menu-roomSlot",
            discussion.roomSlot
              .map(_.displayString)
              .getOrElse("Unscheduled"),
          ),
        ),
      ),
      div(actionSection*),
    )
