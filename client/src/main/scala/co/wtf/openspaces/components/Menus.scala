package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import neotype.*
import co.wtf.openspaces.{Person, RoomSlot, Topic, GitHubAvatar, GlyphiconUtils, SvgIcon}
import co.wtf.openspaces.discussions.Discussion
import co.wtf.openspaces.discussions.DiscussionAction
import co.wtf.openspaces.util.MenuPositioning

private object MenuUi:
  def shell(
    title: String,
    sections: Seq[Modifier[HtmlElement]],
  ): HtmlElement =
    val (x, y) = MenuPositioning.standardMenuPosition()
    val mods = Seq[Modifier[HtmlElement]](
      cls := "Menu",
      left := s"${x}px",
      top := s"${y}px",
      onClick.preventDefault.stopPropagation --> Observer(_ => ()),
      div(cls := "Menu-header", title),
    ) ++ sections
    div(mods)

  def section(
    label: String,
    body: Seq[Modifier[HtmlElement]],
    target: Boolean = false,
  ): HtmlElement =
    val mods = Seq[Modifier[HtmlElement]](
      cls := "Menu-section",
      if target then cls := "Menu-section--target" else emptyMod,
      div(cls := "Menu-label", label),
    ) ++ body
    div(mods)

  def topicRow(
    discussion: Discussion,
    variantClass: String,
  ): HtmlElement =
    div(
      cls := "Menu-topic",
      if variantClass.nonEmpty then cls := variantClass else emptyMod,
      GitHubAvatar(discussion.facilitator),
      div(
        div(cls := "Menu-topicName", discussion.topicName),
        div(
          cls := "Menu-roomSlot",
          discussion.roomSlot
            .map(_.displayString)
            .getOrElse("Unscheduled"),
        ),
      ),
    )

  def actionButton(
    className: String,
    onTap: () => Unit,
    body: Modifier[HtmlElement]*,
  ): HtmlElement =
    val mods = Seq[Modifier[HtmlElement]](
      cls := "open-spaces-button",
      cls := className,
      onClick --> Observer(_ => onTap()),
    ) ++ body
    button(mods)

/** Swap action menu for swapping two scheduled topics */
object Menu:
  def apply(
    selectedDiscussion: Discussion,
    targetDiscussion: Discussion,
    topicUpdates: DiscussionAction => Unit,
    dismissMenu: Observer[Unit],
  ) =
    MenuUi.shell(
      "Actions",
      Seq(
        MenuUi.section(
          "Selected Topic:",
          Seq(MenuUi.topicRow(selectedDiscussion, "Menu-topic--selected")),
        ),
        MenuUi.section(
          "Target Topic:",
          Seq(MenuUi.topicRow(targetDiscussion, "Menu-topic--target")),
          target = true,
        ),
        div(
          cls := "Menu-actions",
          MenuUi.actionButton(
            "Menu-swapButton",
            () =>
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
                case _ => (),
            span("⇅"),
            span("Swap Room Slots"),
          ),
          MenuUi.actionButton(
            "Menu-cancelButton",
            () => dismissMenu.onNext(()),
            "Cancel",
          ),
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
    val textVar = Var("")
    val errorVar = Var[Option[String]](None)

    def submitNewDiscussion() =
      Topic.make(textVar.now()) match
        case Left(error) =>
          errorVar.set(Some(error))
        case Right(topic) =>
          val facilitatorName = facilitator.now()
          if (facilitatorName.unwrap.trim.length < 2) then
            errorVar.set(Some("Please enter your name (2+ characters) before adding."))
          else
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

    val moveCurrentTopicOption: Option[HtmlElement] =
      if currentView != AppView.Admin then None
      else
        activeDiscussion.flatMap { discussion =>
          if discussion.roomSlot.contains(targetRoomSlot) then None
          else
            Some(
              MenuUi.section(
                "Move Current Topic Here:",
                Seq(
                  div(
                    cls := "Menu-actions",
                    MenuUi.actionButton(
                      "Menu-swapButton",
                      () =>
                        topicUpdates(
                          DiscussionAction.SetRoomSlot(
                            discussion.id,
                            discussion.roomSlot,
                            Some(targetRoomSlot),
                          ),
                        )
                        dismissMenu.onNext(()),
                      GitHubAvatar(discussion.facilitator),
                      span(
                        cls := "Menu-topicName",
                        discussion.topicName,
                      ),
                    ),
                  ),
                ),
              ),
            )
        }

    val moveExistingSection =
      if unscheduledDiscussions.isEmpty then
        MenuUi.section(
          "Move Existing Unscheduled Topic:",
          Seq(
            div(
              cls := "Menu-topic",
              div(div(cls := "Menu-topicName", "No unscheduled discussions available")),
            ),
          ),
        )
      else
        MenuUi.section(
          "Move Existing Unscheduled Topic:",
          Seq(
            div(
              cls := "Menu-actions",
              unscheduledDiscussions.map { discussion =>
                MenuUi.actionButton(
                  "Menu-swapButton",
                  () =>
                    topicUpdates(
                      DiscussionAction.SetRoomSlot(
                        discussion.id,
                        discussion.roomSlot,
                        Some(targetRoomSlot),
                      ),
                    )
                    setActiveDiscussion.onNext(
                      discussion.copy(roomSlot = Some(targetRoomSlot)),
                    )
                    dismissMenu.onNext(()),
                  GitHubAvatar(discussion.facilitator),
                  span(
                    cls := "Menu-topicName",
                    discussion.topicName,
                  ),
                )
              },
            ),
          ),
        )

    MenuUi.shell(
      "Assign Discussion",
      Seq(
        MenuUi.section(
          s"Room Slot: ${targetRoomSlot.displayString}",
          Seq.empty,
        ),
        moveCurrentTopicOption.getOrElse(span()),
        MenuUi.section(
          "Create New Discussion:",
          Seq(
            textArea(
              cls := "Menu-textArea",
              placeholder := "Describe the discussion to schedule...",
              value <-- textVar.signal,
              onInput.mapToValue --> textVar,
              onMountCallback(ctx => ctx.thisNode.ref.focus()),
            ),
            MenuUi.actionButton(
              "Menu-swapButton",
              () => submitNewDiscussion(),
              "Add & Assign",
            ),
            child <-- errorVar.signal.map {
              case Some(errorMessage) => div(cls := "Menu-error", errorMessage)
              case None => span()
            },
          ),
        ),
        moveExistingSection,
        div(
          cls := "Menu-actions",
          MenuUi.actionButton(
            "Menu-cancelButton",
            () => dismissMenu.onNext(()),
            "Cancel",
          ),
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
    val actionSection: HtmlElement =
      if discussion.roomSlot.isDefined then
        div(
          cls := "Menu-actions",
          MenuUi.actionButton(
            "Menu-swapButton",
            () =>
              topicUpdates(
                DiscussionAction.SetRoomSlot(
                  discussion.id,
                  discussion.roomSlot,
                  None,
                ),
              )
              dismissMenu.onNext(()),
            SvgIcon(GlyphiconUtils.minus),
            span("Unschedule topic"),
          ),
          MenuUi.actionButton(
            "Menu-cancelButton",
            () => dismissMenu.onNext(()),
            "Close",
          ),
        )
      else
        div(
          cls := "Menu-actions",
          div(
            cls := "Menu-topic Menu-topic--selected",
            span(
              cls := "Menu-topicName",
              "This discussion is not scheduled yet.",
            ),
          ),
          MenuUi.actionButton(
            "Menu-cancelButton",
            () => dismissMenu.onNext(()),
            "Close",
          ),
        )

    MenuUi.shell(
      "Discussion actions",
      Seq(
        MenuUi.topicRow(discussion, "Menu-topic--selected"),
        actionSection,
      ),
    )
