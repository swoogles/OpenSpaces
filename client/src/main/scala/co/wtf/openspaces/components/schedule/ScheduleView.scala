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
import co.wtf.openspaces.components.activities.{ActivityCard, NewActivityForm}
import co.wtf.openspaces.components.discussions.TopicCard
import co.wtf.openspaces.components.schedule.ScheduleSlotComponent
import co.wtf.openspaces.ConnectionStatusUI
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
  import scala.scalajs.js

  private def lightningNightForDay(dayName: String): Option[LightningTalkNight] =
    dayName match
      case "TUESDAY"   => Some(LightningTalkNight.Tuesday)
      case "THURSDAY"  => Some(LightningTalkNight.Thursday)
      case _           => None

  /** Generate a unique slot ID for scrolling (discussion slots). */
  private def slotId(startTime: LocalDateTime): String =
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd-HH-mm")
    s"slot-${startTime.format(formatter)}"

  /** Generate a unique activity ID for scrolling. */
  private def activitySlotId(eventTime: LocalDateTime): String =
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd-HH-mm")
    s"activity-${eventTime.format(formatter)}"

  /** Find the next upcoming rendered item (discussion slot or activity). */
  private def findNextUpcomingId(state: DiscussionState, activities: List[Activity]): Option[String] =
    val now = LocalDateTime.now()
    
    val nextSlot: Option[(LocalDateTime, String)] = state.slots
      .flatMap(_.slots)
      .collect {
        case slot if slot.time.endTime.isAfter(now) =>
            (slot.time.startTime, slotId(slot.time.startTime))
      }
      .sortBy(_._1)
      .headOption
    
    // Find next activity (by event time)
    val nextActivity: Option[(LocalDateTime, String)] = activities
      .filter(_.eventTime.isAfter(now))
      .sortBy(_.eventTime)
      .headOption
      .map(a => (a.eventTime, activitySlotId(a.eventTime)))
    
    // Return whichever comes first
    (nextSlot, nextActivity) match
      case (Some((slotTime, slotId)), Some((actTime, actId))) =>
        if actTime.isBefore(slotTime) then Some(actId) else Some(slotId)
      case (Some((_, id)), None) => Some(id)
      case (None, Some((_, id))) => Some(id)
      case (None, None) => None

  /** Find the first rendered item in schedule order (discussion slot or activity). */
  private def findFirstRenderedId(state: DiscussionState, activities: List[Activity]): Option[String] =
    val firstRenderedSlot: Option[(LocalDateTime, String)] = state.slots
      .flatMap(_.slots)
      .collect {
        case slot => (slot.time.startTime, slotId(slot.time.startTime))
      }
      .sortBy(_._1)
      .headOption

    val firstActivity: Option[(LocalDateTime, String)] = activities
      .sortBy(_.eventTime)
      .headOption
      .map(a => (a.eventTime, activitySlotId(a.eventTime)))

    (firstRenderedSlot, firstActivity) match
      case (Some((slotTime, slotDomId)), Some((actTime, actDomId))) =>
        if actTime.isBefore(slotTime) then Some(actDomId) else Some(slotDomId)
      case (Some((_, id)), None) => Some(id)
      case (None, Some((_, id))) => Some(id)
      case (None, None) => None

  /** Scroll smoothly to the next upcoming item. */
  private def scrollToNextItem(state: DiscussionState, activities: List[Activity]): Unit =
    import scala.scalajs.js
    findNextUpcomingId(state, activities)
      .orElse(findFirstRenderedId(state, activities))
      .foreach { targetId =>
      Option(dom.document.getElementById(targetId)).foreach { element =>
        element.asInstanceOf[js.Dynamic].scrollIntoView(
          js.Dynamic.literal(behavior = "smooth", block = "center")
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
    connectionStatus: ConnectionStatusUI,
    unscheduledMenuState: Var[Option[RoomSlot]],
  ): HtmlElement =
    val _ = unscheduledMenuState

    val activityDisplayFormat = DateTimeFormatter.ofPattern("EEE h:mm a")
    val activityHeaderTimeFormat = DateTimeFormatter.ofPattern("h:mm a")

    // Local vars to hold current state for onClick handler
    val currentStateVar = Var(DiscussionState.empty)
    val currentActivityStateVar = Var(ActivityState.empty)
    
    // State for inline activity creation
    val showCreateActivityForm = Var(false)
    
    // State for unified context header (day + time)
    val currentDayVar = Var[Option[String]](None)
    val currentTimeVar = Var[Option[String]](None)
    
    // Scroll progress (0.0 to 1.0)
    val scrollProgress = Var(0.0)
    
    // Reference to the sticky header for calculating intersection margins
    val stickyHeaderRef = Var[Option[dom.Element]](None)

    div(
      cls := "LinearScheduleView",
      // Keep state vars in sync with the signals
      $discussionState --> currentStateVar.writer,
      $activityState --> currentActivityStateVar.writer,
      // Sticky container for buttons, context header, AND form
      div(
        cls := "LinearScheduleView-stickyHeader",
        onMountCallback { ctx =>
          stickyHeaderRef.set(Some(ctx.thisNode.ref))
        },
        // Button row
        div(
          cls := "LinearScheduleView-buttonRow",
          button(
            cls := "JumpToNowButton",
            SvgIcon(GlyphiconUtils.schedule),
            span("Jump to Now"),
            onClick --> Observer { _ =>
              val activities = currentActivityStateVar.now().activities.values.toList
              scrollToNextItem(currentStateVar.now(), activities)
            },
          ),
          child <-- showCreateActivityForm.signal.map {
            case false =>
              button(
                cls := "AddActivityButton",
                "+",
                title := "Propose Activity",
                onClick --> Observer(_ => showCreateActivityForm.set(true)),
              )
            case true =>
              emptyNode
          },
        ),
        // Unified context header (day on left, time on right, progress bar at bottom)
        child <-- Signal.combine(currentDayVar.signal, currentTimeVar.signal, scrollProgress.signal).map {
          case (Some(day), timeOpt, progress) =>
            div(
              cls := "LinearContextHeader",
              div(cls := "LinearContextHeader-day", day),
              div(
                cls := "LinearContextHeader-time",
                timeOpt.getOrElse("")
              ),
              div(
                cls := "LinearContextHeader-progressBar",
                div(
                  cls := "LinearContextHeader-progressFill",
                  styleAttr := s"width: ${(progress * 100).toInt}%"
                )
              )
            )
          case _ => emptyNode
        },
        // Inline activity creation form (inside sticky container)
        child <-- showCreateActivityForm.signal.map {
          case true =>
            div(
              cls := "LinearScheduleView-inlineForm",
              NewActivityForm(
                name = name,
                sendActivityAction = sendActivityAction,
                setErrorMsg = setErrorMsg,
                connectionStatus = connectionStatus,
                onClose = () => showCreateActivityForm.set(false),
                compact = true,
              ),
            )
          case false =>
            emptyNode
        },
      ),
      // Main content area - scroll-based tracking for day/time/progress
      div(
        cls := "LinearScheduleView-content",
        onMountCallback { ctx =>
          val contentEl = ctx.thisNode.ref
          
          // Unified scroll handler for progress bar AND day/time tracking
          val scrollHandler: js.Function1[dom.Event, Unit] = { (_: dom.Event) =>
            val docEl = dom.document.documentElement
            val scrollTop = dom.window.pageYOffset
            val scrollHeight = docEl.scrollHeight - docEl.clientHeight
            
            // Update progress bar
            val progress = if scrollHeight > 0 then (scrollTop / scrollHeight).min(1.0).max(0.0) else 0.0
            scrollProgress.set(progress)
            
            // Calculate header height for threshold
            val headerHeight = stickyHeaderRef.now()
              .map(_.asInstanceOf[dom.html.Element].offsetHeight)
              .getOrElse(80.0)
            val threshold = headerHeight + 20 // A bit below the sticky header
            
            // Find which day is currently in view (last one whose top is above threshold)
            val dayElements = contentEl.querySelectorAll("[data-day]")
            var currentDay: Option[String] = None
            dayElements.foreach { el =>
              val rect = el.asInstanceOf[dom.Element].getBoundingClientRect()
              if rect.top <= threshold then
                val dayName = el.asInstanceOf[dom.Element].getAttribute("data-day")
                if dayName != null && dayName.nonEmpty then
                  currentDay = Some(dayName)
            }
            currentDay.foreach(d => currentDayVar.set(Some(d)))
            
            // Find which time slot is currently in view
            val timeElements = contentEl.querySelectorAll("[data-time]")
            var currentTime: Option[String] = None
            timeElements.foreach { el =>
              val rect = el.asInstanceOf[dom.Element].getBoundingClientRect()
              if rect.top <= threshold then
                val timeText = el.asInstanceOf[dom.Element].getAttribute("data-time")
                if timeText != null && timeText.nonEmpty then
                  currentTime = Some(timeText)
            }
            currentTime.foreach(t => currentTimeVar.set(Some(t)))
          }
          
          dom.window.addEventListener("scroll", scrollHandler)
          
          // Initialize on mount
          dom.window.setTimeout(() => scrollHandler(null), 100)
        },
        children <-- Signal.combine($discussionState, $lightningTalkState, $activityState).map { (state, lightningState, activityState) =>
          val daySlotsByDate = state.slots.map(ds => ds.date -> ds).toMap
          val activityDates = activityState.activities.values.map(_.eventTime.toLocalDate).toSet
          val allDates = (daySlotsByDate.keySet ++ activityDates).toList.sortBy(_.toEpochDay)

          allDates.map { date =>
            val daySlotOpt = daySlotsByDate.get(date)
            val dayName = date.getDayOfWeek.toString
            val maybeLightningNight = lightningNightForDay(dayName)
            val dayActivities = activityState.activities.values
              .filter(_.eventTime.toLocalDate == date)
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
                idAttr := activitySlotId(activity.eventTime),
                dataAttr("time") := activity.eventTime.format(activityHeaderTimeFormat),
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
              dataAttr("day") := dayName,
              daySlotOpt.toList.flatMap(_.slots).flatMap { timeSlotForAllRooms =>
                val leadingActivities = consumeActivitiesThrough(timeSlotForAllRooms.time.startTime).map(renderActivity)
                val scheduledRooms = timeSlotForAllRooms.rooms.flatMap { room =>
                  val roomSlot = RoomSlot(room, timeSlotForAllRooms.time)
                  state.roomSlotContent(roomSlot).map(discussion => (room, discussion))
                }

                val slot =
                  div(
                    cls := "LinearTimeSlot",
                    idAttr := slotId(timeSlotForAllRooms.time.startTime),
                    dataAttr("time") := timeSlotForAllRooms.time.displayString,
                    if scheduledRooms.isEmpty then
                      div(
                        cls := "LinearEmptyTimeSlotMessage",
                        "No topics scheduled yet",
                      )
                    else
                      scheduledRooms.map { (room, discussion) =>
                        div(
                          cls := "LinearRoomSlot",
                          div(cls := "LinearRoomName", room.name),
                          div(
                            child <-- TopicCard(
                              name,
                              topicUpdates,
                              Signal.fromValue(Some(discussion)),
                              isAdmin,
                              connectionStatus,
                            ),
                          ),
                        )
                      },
                  )
                leadingActivities :+ slot
              } ++ consumeRemainingActivities().map(renderActivity) ++ daySlotOpt.toList.flatMap(_ => maybeLightningNight.toList).map { _ =>
                div(
                  cls := "LinearTimeSlot LinearTimeSlot--lightning",
                  dataAttr("time") := "8:00 PM Lightning Talks",
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
      ),
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
