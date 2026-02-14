package co.wtf.openspaces.components

import animus.*
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

import co.wtf.openspaces.{
  Discussion, DiscussionAction, DiscussionState, Person, RoomSlot, TimeSlotForAllRooms,
  VotingState, GitHubAvatar, SvgIcon, GlyphiconUtils
}
import co.wtf.openspaces.util.{SlotPositionTracker, SwapAnimationState}

/** Schedule slot component showing topic avatar with swap/move support.
  *
  * Extracted from FrontEnd.scala for better code organization.
  */
object ScheduleSlotComponent:
  def apply(
    roomSlot: RoomSlot,
    $slotContent: Signal[Option[Discussion]],
    updateDiscussion: Observer[Discussion],
    $activeDiscussion: StrictSignal[Option[Discussion]],
    showPopover: Observer[Discussion],
    showSwapMenu: Observer[(Discussion, Discussion)],
    showUnscheduledMenu: Observer[RoomSlot],
    topicUpdates: DiscussionAction => Unit,
    name: StrictSignal[Person],
  ): HtmlElement =
    // Keep unregister handle across mount/unmount
    var unregisterSlot: () => Unit = () => ()
    span(
      // Track this slot's DOM position so move animations can start from the
      // actual on-screen slot instead of an approximate grid offset
      onMountUnmountCallback(
        ctx =>
          val element = ctx.thisNode.ref
          unregisterSlot =
            SlotPositionTracker.register(roomSlot, element)
        ,
        _ => unregisterSlot(),
      ),
      // Combine slot content with active discussion to render appropriately
      child <-- $slotContent.combineWith($activeDiscussion).map { case (slotContentOpt, discussionO) =>
        slotContentOpt match
          case Some(value) =>
            val selectedTopicStyling =
              if ($activeDiscussion.now().map(_.id).contains(value.id))
                "activeTopicIcon"
              else ""
            // Check if there's an active discussion that's different from this slot's topic
            val isSwappable = discussionO.exists(active =>
              active.id != value.id && active.roomSlot.isDefined,
            )

            // Get the animated offset for this topic (if it's part of a swap)
            val $offset = SwapAnimationState.getOffsetSignal(value.id)
            // Use spring animation for smooth movement
            val $animatedX = $offset.map(_._1).spring
            val $animatedY = $offset.map(_._2).spring

            // Calculate voting state for styling
            val votingState = VotingState.forUser(value, name.now())

            span(
              cls := "swap-topic-icon",
              // Apply animated transform based on swap offset
              transform <-- $animatedX.combineWith($animatedY).map {
                case (x, y) =>
                  if (x != 0.0 || y != 0.0) s"translate(${x}px, ${y}px)"
                  else "none"
              },
              onClick.stopPropagation.mapTo(value) --> showPopover,
              // Long-press to show swap menu when there's an active discussion
              if (isSwappable)
                onContextMenu.preventDefault --> Observer { (event: org.scalajs.dom.MouseEvent) =>
                  event.stopPropagation()
                  discussionO.foreach { activeDiscussion =>
                    showSwapMenu.onNext((activeDiscussion, value))
                  }
                }
              else emptyMod,
              onClick.mapTo(value) --> updateDiscussion,
              GitHubAvatar.withVotingState(
                value.facilitator,
                votingState,
                value.topicName,
                s"filledTopic $selectedTopicStyling",
              ),
            )
          case None =>
            discussionO match
              case Some(discussion) =>
                discussion.roomSlot match
                  case Some(value) if roomSlot == value =>
                    GitHubAvatar(discussion.facilitator, "filledTopic")
                  case Some(_) =>
                    // Empty slot when active discussion is scheduled elsewhere
                    span(
                      cls := "emptySlotWithActiveDiscussion",
                      SvgIcon(GlyphiconUtils.emptySlot),
                      onClick.stopPropagation.mapTo(roomSlot) --> showUnscheduledMenu,
                    )
                  case None =>
                    // Empty slot when active discussion is unscheduled
                    span(
                      cls := "emptySlotWithActiveDiscussion",
                      SvgIcon(GlyphiconUtils.plus),
                      onClick.stopPropagation.mapTo(roomSlot) --> showUnscheduledMenu,
                    )
              case None =>
                // Empty slot with no active discussion - show menu on click
                span(
                  SvgIcon(GlyphiconUtils.emptySlot),
                  onClick.stopPropagation.mapTo(roomSlot) --> showUnscheduledMenu,
                )
      },
    )

/** Row of schedule slots for a single time slot across all rooms.
  */
object SlotSchedule:
  def apply(
    $discussionState: Signal[DiscussionState],
    timeSlotsForAllRooms: TimeSlotForAllRooms,
    updateDiscussion: Observer[Discussion],
    activeDiscussion: StrictSignal[Option[Discussion]],
    showPopover: Observer[Discussion],
    showSwapMenu: Observer[(Discussion, Discussion)],
    showUnscheduledMenu: Observer[RoomSlot],
    topicUpdates: DiscussionAction => Unit,
    name: StrictSignal[Person],
  ): HtmlElement =
    div(
      cls := "SlotRow",
      div(cls := "TimeOfSlot", timeSlotsForAllRooms.time.s),
      timeSlotsForAllRooms.rooms.map { room =>
        val roomSlot = RoomSlot(room, timeSlotsForAllRooms.time)
        val $slotContent = $discussionState.map(_.roomSlotContent(roomSlot))
        div(
          cls := "Cell",
          ScheduleSlotComponent(
            roomSlot,
            $slotContent,
            updateDiscussion,
            activeDiscussion,
            showPopover,
            showSwapMenu,
            showUnscheduledMenu,
            topicUpdates,
            name,
          ),
        )
      },
    )
