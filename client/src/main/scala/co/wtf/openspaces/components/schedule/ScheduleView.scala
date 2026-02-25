package co.wtf.openspaces.components

import animus.*
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import neotype.unwrap

import co.wtf.openspaces.{
  Person, Room, RoomSlot,
  SvgIcon, GlyphiconUtils
}
import co.wtf.openspaces.discussions.DaySlots
import co.wtf.openspaces.discussions.Discussion
import co.wtf.openspaces.discussions.{DiscussionAction, DiscussionState}
import co.wtf.openspaces.lighting_talks.{LightningTalkNight, LightningTalkProposal, LightningTalkState}
import co.wtf.openspaces.activities.{Activity, ActivityAction, ActivityState}
import co.wtf.openspaces.util.ScrollPreserver
import co.wtf.openspaces.FrontEnd.connectionStatus
import co.wtf.openspaces.components.lightning_talks.LightningTalkProposalCard
import co.wtf.openspaces.components.activities.ActivityCard
import co.wtf.openspaces.components.discussions.TopicCard
import co.wtf.openspaces.components.schedule.ScheduleSlotComponent
import java.time.format.DateTimeFormatter
import java.time.LocalDateTime

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
    isAdmin: Signal[Boolean],
    updateTargetDiscussion: Observer[Discussion],
    popoverState: Var[Option[Discussion]],
    swapMenuState: Var[Option[(Discussion, Discussion)]],
    unscheduledMenuState: Var[Option[RoomSlot]],
    activeDiscussionMenuState: Var[Option[Discussion]],
  ): HtmlElement =
    // Derive rooms reactively from current state (rooms should be same across all slots)
    val $rooms: Signal[List[Room]] = fullSchedule.signal.map { state =>
      state.slots.headOption
        .flatMap(_.slots.headOption)
        .map(_.rooms)
        .getOrElse(Nil)
    }

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
            isAdmin,
            connectionStatus,
          ),
        ),
      ),
      div(
        cls := "Schedule",
        div(
          cls := "RoomHeaders",
          children <-- $rooms.map(rooms => rooms.map(room => div(cls := "RoomHeader", room.name))),
        ),
        div(
          cls := "TimeSlots",
          ScrollPreserver.timeSlotsIdAttr,
          child <-- fullSchedule.signal.map { state =>
            SlotSchedules(
              state.slots,
              fullSchedule.signal,
              updateTargetDiscussion,
              activeDiscussion.signal,
              showPopover,
              showSwapMenu,
              showUnscheduledMenu,
              topicUpdates,
              name,
            )
          },
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
              div(cls := "TimeOfSlot", timeSlotsForAllRooms.time.displayString),
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

  /** Generate a unique slot ID for scrolling. */
  private def slotId(startTime: LocalDateTime): String =
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd-HH-mm")
    s"slot-${startTime.format(formatter)}"

  /** Find the next upcoming slot (first slot whose end time is in the future). */
  private def findNextUpcomingSlotId(slots: List[DaySlots]): Option[String] =
    val now = LocalDateTime.now()
    slots
      .flatMap(_.slots)
      .find(_.time.endTime.isAfter(now))
      .map(slot => slotId(slot.time.startTime))

  /** Scroll smoothly to the next upcoming slot. */
  private def scrollToNextSlot(slots: List[DaySlots]): Unit =
    import scala.scalajs.js
    findNextUpcomingSlotId(slots).foreach { targetId =>
      Option(dom.document.getElementById(targetId)).foreach { element =>
        element.asInstanceOf[js.Dynamic].scrollIntoView(
          js.Dynamic.literal(behavior = "smooth", block = "start")
        )
      }
    }

  def apply(
    $discussionState: Signal[DiscussionState],
    $lightningTalkState: Signal[LightningTalkState],
    $activityState: Signal[ActivityState],
    topicUpdates: DiscussionAction => Unit,
    sendActivityAction: ActivityAction => Unit,
    name: StrictSignal[Person],
    isAdmin: Signal[Boolean],
    setErrorMsg: Observer[Option[String]],
    unscheduledMenuState: Var[Option[RoomSlot]],
  ): HtmlElement =
    val showUnscheduledMenu: Observer[RoomSlot] =
      Observer { roomSlot =>
        unscheduledMenuState.set(Some(roomSlot))
      }

    val activityDisplayFormat = DateTimeFormatter.ofPattern("EEE h:mm a")
    val activityHeaderTimeFormat = DateTimeFormatter.ofPattern("h:mm a")

    // Local var to hold current state for onClick handler
    val currentStateVar = Var(DiscussionState.empty)

    div(
      cls := "LinearScheduleView",
      // Keep currentStateVar in sync with the signal
      $discussionState --> currentStateVar.writer,
      // Jump to Now button
      div(
        cls := "LinearScheduleView-jumpButton",
        button(
          cls := "JumpToNowButton",
          SvgIcon(GlyphiconUtils.schedule),
          span("Jump to Now"),
          onClick --> Observer { _ =>
            scrollToNextSlot(currentStateVar.now().slots)
          },
        ),
      ),
      children <-- Signal.combine($discussionState, $lightningTalkState, $activityState).map { (state, lightningState, activityState) =>
        state.slots.map { daySlot =>
          val dayName = daySlot.date.getDayOfWeek.toString
          val maybeLightningNight = lightningNightForDay(dayName)
          val dayActivities = activityState.activities.values
            .filter(_.eventTime.toLocalDate == daySlot.date)
            .toList
            .sortBy(activity =>
              (
                activity.eventTime,
                -activity.interestCount,
                activity.createdAtEpochMs,
                activity.id.unwrap,
              ),
            )
          val lightningSlotMap = maybeLightningNight
            .map(night =>
              lightningState
                .assignedForNight(night)
                .flatMap(proposal => proposal.assignment.map(_.slot.unwrap -> proposal))
                .toMap,
            )
            .getOrElse(Map.empty[Int, LightningTalkProposal])
          var nextActivityIndex = 0

          def consumeActivitiesThrough(slotStart: java.time.LocalDateTime): List[Activity] =
            val buffer = scala.collection.mutable.ListBuffer.empty[Activity]
            while nextActivityIndex < dayActivities.length && !dayActivities(nextActivityIndex).eventTime.isAfter(slotStart) do
              buffer += dayActivities(nextActivityIndex)
              nextActivityIndex += 1
            buffer.toList

          def consumeRemainingActivities(): List[Activity] =
            if nextActivityIndex >= dayActivities.length then Nil
            else
              val rest = dayActivities.drop(nextActivityIndex)
              nextActivityIndex = dayActivities.length
              rest

          def renderActivity(activity: Activity): HtmlElement =
            div(
              cls := "LinearTimeSlot LinearTimeSlot--activity",
              div(cls := "LinearTimeHeader", activity.eventTime.format(activityHeaderTimeFormat)),
              ActivityCard(
                activity = activity,
                currentUser = name.now(),
                sendActivityAction = sendActivityAction,
                connectionStatus = connectionStatus,
                setErrorMsg = setErrorMsg,
                displayFormat = activityDisplayFormat,
              ),
            )

          div(
            cls := "LinearDay",
            div(cls := "LinearDayHeader", dayName),
            daySlot.slots.flatMap { timeSlotForAllRooms =>
              val leadingActivities = consumeActivitiesThrough(timeSlotForAllRooms.time.startTime).map(renderActivity)

              val slot =
              div(
                cls := "LinearTimeSlot",
                idAttr := slotId(timeSlotForAllRooms.time.startTime),
                div(cls := "LinearTimeHeader", timeSlotForAllRooms.time.displayString),
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
                            isAdmin,
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
              leadingActivities :+ slot
            } ++ consumeRemainingActivities().map(renderActivity) ++ maybeLightningNight.toList.map { _ =>
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
