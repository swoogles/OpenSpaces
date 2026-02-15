package co.wtf.openspaces.components

import animus.*
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

import co.wtf.openspaces.{
  Discussion, DiscussionAction, DiscussionState, Person, RoomSlot,
  GitHubAvatar, SvgIcon, GlyphiconUtils
}
import co.wtf.openspaces.util.ScrollPreserver
import co.wtf.openspaces.FrontEnd.connectionStatus

/** App view mode enum.
  */
enum AppView:
  case Admin
  case Topics
  case Schedule

/** Schedule view showing the full grid of rooms and time slots.
  *
  * Extracted from FrontEnd.scala for better code organization.
  */
object ScheduleView:
  def apply(
    fullSchedule: Var[DiscussionState],
    activeDiscussion: Var[Option[Discussion]],
    topicUpdates: DiscussionAction => Unit,
    name: StrictSignal[Person],
    updateTargetDiscussion: Observer[Discussion],
    popoverState: Var[Option[Discussion]],
    swapMenuState: Var[Option[(Discussion, Discussion)]],
    unscheduledMenuState: Var[Option[RoomSlot]],
    activeDiscussionMenuState: Var[Option[Discussion]],
  ): HtmlElement =
    val showPopover: Observer[Discussion] =
      Observer { discussion =>
        popoverState.set(Some(discussion))
      }

    val showSwapMenu: Observer[(Discussion, Discussion)] =
      Observer { case (selected, target) =>
        swapMenuState.set(Some((selected, target)))
      }

    val showUnscheduledMenu: Observer[RoomSlot] =
      Observer { roomSlot =>
        unscheduledMenuState.set(Some(roomSlot))
      }

    val showActiveDiscussionMenu: Observer[Discussion] =
      Observer { discussion =>
        activeDiscussionMenuState.set(Some(discussion))
      }

    val handleActiveDiscussionLongPress =
      activeDiscussionLongPressBinder(() => activeDiscussion.now(),
                                      showActiveDiscussionMenu,
      )

    div(
      cls := "container",
      div(
        cls := "Targets",
        div(
          cls := "ActiveDiscussion Topic",
          handleActiveDiscussionLongPress,
          child <-- TopicCard(
            name,
            topicUpdates,
            activeDiscussion.signal,
            connectionStatus,
            iconModifiers = Seq(handleActiveDiscussionLongPress),
          ),
        ),
      ),
      div(
        cls := "Schedule",
        div(
          cls := "RoomHeaders",
          div(cls := "Room1", "King"),
          div(cls := "Room2", "Hawk"),
          div(cls := "Room3", "Art"),
          div(cls := "Room4", "Dance"),
        ),
        div(
          cls := "TimeSlots",
          ScrollPreserver.timeSlotsIdAttr,
          SlotSchedules(
            fullSchedule.signal,
            updateTargetDiscussion,
            activeDiscussion.signal,
            showPopover,
            showSwapMenu,
            showUnscheduledMenu,
            topicUpdates,
            name,
          ),
        ),
      ),
    )

/** Grid of all schedule slots organized by day and time.
  */
object SlotSchedules:
  def apply(
    $discussionState: Signal[DiscussionState],
    updateDiscussion: Observer[Discussion],
    activeDiscussion: StrictSignal[Option[Discussion]],
    showPopover: Observer[Discussion],
    showSwapMenu: Observer[(Discussion, Discussion)],
    showUnscheduledMenu: Observer[RoomSlot],
    topicUpdates: DiscussionAction => Unit,
    name: StrictSignal[Person],
  ): HtmlElement =
    // Build the grid structure statically - slots don't change, only their content does
    val slots = DiscussionState.timeSlotExamples
    div(
      slots.map { daySlot =>
        div(
          div(cls := "DayHeader", daySlot.date.getDayOfWeek.toString().take(3)),
          daySlot.slots.map { timeSlotsForAllRooms =>
            div(
              cls := "SlotRow",
              div(cls := "TimeOfSlot", timeSlotsForAllRooms.time.s),
              timeSlotsForAllRooms.rooms.map { room =>
                val roomSlot = RoomSlot(room, timeSlotsForAllRooms.time)
                // Derive a signal that only emits when THIS slot's content changes
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
          },
        )
      },
    )

/** Linear schedule view showing full topic cards organized by day/time/room.
  */
object LinearScheduleView:
  def apply(
    $discussionState: Signal[DiscussionState],
    topicUpdates: DiscussionAction => Unit,
    name: StrictSignal[Person],
    unscheduledMenuState: Var[Option[RoomSlot]],
  ): HtmlElement =
    val showUnscheduledMenu: Observer[RoomSlot] =
      Observer { roomSlot =>
        unscheduledMenuState.set(Some(roomSlot))
      }

    div(
      cls := "LinearScheduleView",
      children <-- $discussionState.map { state =>
        state.slots.map { daySlot =>
          div(
            cls := "LinearDay",
            div(cls := "LinearDayHeader", daySlot.date.getDayOfWeek.toString),
            daySlot.slots.map { timeSlotForAllRooms =>
              div(
                cls := "LinearTimeSlot",
                div(cls := "LinearTimeHeader", timeSlotForAllRooms.time.s),
                timeSlotForAllRooms.rooms.map { room =>
                  val roomSlot = RoomSlot(room, timeSlotForAllRooms.time)
                  val discussion = state.roomSlotContent(roomSlot)
                  div(
                    cls := "LinearRoomSlot",
                    div(cls := "LinearRoomName", room.name),
                    discussion match {
                      case Some(disc) =>
                        div(
                          child <-- TopicCard(
                            name,
                            topicUpdates,
                            Signal.fromValue(Some(disc)),
                            connectionStatus,
                          ),
                        )
                      case None =>
                        div(
                          cls := "LinearEmptySlot",
                          cursor := "pointer",
                          onClick.stopPropagation.mapTo(roomSlot) --> showUnscheduledMenu,
                          SvgIcon(GlyphiconUtils.plus),
                          span("Add topic"),
                        )
                    },
                  )
                },
              )
            },
          )
        }
      },
    )

/** Long-press binder for active discussion context menu.
  */
def activeDiscussionLongPressBinder(
  activeDiscussionNow: () => Option[Discussion],
  showActiveDiscussionMenu: Observer[Discussion],
): Binder[HtmlElement] =
  onContextMenu.preventDefault --> Observer {
    (event: org.scalajs.dom.MouseEvent) =>
      event.stopPropagation()
      activeDiscussionNow().foreach { discussion =>
        if (discussion.roomSlot.isDefined) {
          showActiveDiscussionMenu.onNext(discussion)
        }
      }
  }
