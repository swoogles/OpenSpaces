package co.wtf.openspaces.components

import animus.*
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import neotype.unwrap

import co.wtf.openspaces.{
  DaySlots, Discussion, Person, RoomSlot,
  SvgIcon, GlyphiconUtils
}
import co.wtf.openspaces.discussions.{DiscussionAction, DiscussionState}
import co.wtf.openspaces.lighting_talks.{LightningTalkNight, LightningTalkProposal, LightningTalkState}
import co.wtf.openspaces.util.ScrollPreserver
import co.wtf.openspaces.FrontEnd.connectionStatus

/** App view mode enum.
  */
enum AppView:
  case Admin
  case Topics
  case Schedule
  case LightningTalks
  case Hackathon
  case Replay

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
    val rooms = fullSchedule
      .now()
      .slots
      .headOption
      .flatMap(_.slots.headOption)
      .map(_.rooms)
      .getOrElse(Nil)

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
          cls := "ActiveDiscussion",
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
          rooms.map(room => div(cls := "RoomHeader", room.name)),
        ),
        div(
          cls := "TimeSlots",
          ScrollPreserver.timeSlotsIdAttr,
          SlotSchedules(
            fullSchedule.now().slots,
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
    slots: List[DaySlots],
    $discussionState: Signal[DiscussionState],
    updateDiscussion: Observer[Discussion],
    activeDiscussion: StrictSignal[Option[Discussion]],
    showPopover: Observer[Discussion],
    showSwapMenu: Observer[(Discussion, Discussion)],
    showUnscheduledMenu: Observer[RoomSlot],
    topicUpdates: DiscussionAction => Unit,
    name: StrictSignal[Person],
  ): HtmlElement =
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
  private def lightningNightForDay(dayName: String): Option[LightningTalkNight] =
    dayName match
      case "TUESDAY"   => Some(LightningTalkNight.Tuesday)
      case "THURSDAY"  => Some(LightningTalkNight.Thursday)
      case _           => None

  def apply(
    $discussionState: Signal[DiscussionState],
    $lightningTalkState: Signal[LightningTalkState],
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
      children <-- Signal.combine($discussionState, $lightningTalkState).map { (state, lightningState) =>
        state.slots.map { daySlot =>
          val dayName = daySlot.date.getDayOfWeek.toString
          val maybeLightningNight = lightningNightForDay(dayName)
          val lightningSlotMap = maybeLightningNight
            .map(night =>
              lightningState
                .assignedForNight(night)
                .flatMap(proposal => proposal.assignment.map(_.slot.unwrap -> proposal))
                .toMap,
            )
            .getOrElse(Map.empty[Int, LightningTalkProposal])
          div(
            cls := "LinearDay",
            div(cls := "LinearDayHeader", dayName),
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
                          onClick.stopPropagation.mapTo(roomSlot) --> showUnscheduledMenu,
                          SvgIcon(GlyphiconUtils.plus),
                          span("Add topic"),
                        )
                    },
                  )
                },
              )
            } ++ maybeLightningNight.toList.map { _ =>
              div(
                cls := "LinearTimeSlot LinearTimeSlot--lightning",
                div(
                  cls := "LinearTimeHeader LinearTimeHeader--lightning",
                  "Evening Lightning Talks",
                ),
                div(
                  cls := "LightningTalk-night LightningTalk-night--schedule",
                  (1 to 10).toList.map { slotNumber =>
                    val assigned = lightningSlotMap.get(slotNumber)
                    div(
                      assigned match
                        case Some(proposal) =>
                          LightningTalkProposalCard(
                            proposal = proposal,
                            metaText = None,
                            rowClass = "LightningTalk-slot",
                            slotNumber = Some(slotNumber),
                          )
                        case None =>
                          div(
                            cls := "LightningTalk-slot",
                            div(cls := "LightningTalk-slotNumber", s"#$slotNumber"),
                            div(
                              cls := "LightningTalk-main LightningTalk-main--empty",
                              "Open slot",
                            ),
                          ),
                    )
                  },
                ),
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
