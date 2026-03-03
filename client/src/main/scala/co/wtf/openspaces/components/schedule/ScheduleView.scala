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
  import scala.scalajs.js.annotation.JSGlobal

  @js.native
  @JSGlobal
  class IntersectionObserver(callback: js.Function2[js.Array[IntersectionObserverEntry], IntersectionObserver, Unit], options: js.UndefOr[IntersectionObserverInit] = js.undefined) extends js.Object:
    def observe(target: dom.Element): Unit = js.native
    def unobserve(target: dom.Element): Unit = js.native
    def disconnect(): Unit = js.native

  trait IntersectionObserverEntry extends js.Object:
    val target: dom.Element
    val isIntersecting: Boolean
    val boundingClientRect: dom.DOMRect
    val intersectionRatio: Double

  trait IntersectionObserverInit extends js.Object:
    val root: js.UndefOr[dom.Element]
    val rootMargin: js.UndefOr[String]
    val threshold: js.UndefOr[Double | js.Array[Double]]

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
        // Unified context header (day on left, time on right)
        child <-- Signal.combine(currentDayVar.signal, currentTimeVar.signal).map {
          case (Some(day), timeOpt) =>
            div(
              cls := "LinearContextHeader",
              div(cls := "LinearContextHeader-day", day),
              div(
                cls := "LinearContextHeader-time",
                timeOpt.getOrElse("")
              ),
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
      // Main content area with intersection observer setup
      div(
        cls := "LinearScheduleView-content",
        onMountCallback { ctx =>
          // Set up IntersectionObserver for day and time tracking
          val contentEl = ctx.thisNode.ref
          
          // Calculate the sticky header height for the root margin
          val headerHeight = stickyHeaderRef.now().map(_.asInstanceOf[dom.html.Element].offsetHeight).getOrElse(52.0)
          
          // Day observer - tracks which day section is at the top
          val dayObserverOptions = js.Dynamic.literal(
            rootMargin = s"-${headerHeight.toInt}px 0px -90% 0px",
            threshold = 0.0
          ).asInstanceOf[IntersectionObserverInit]
          
          val dayObserver = new IntersectionObserver(
            { (entries, _) =>
              entries.foreach { entry =>
                if entry.isIntersecting then
                  val dayName = entry.target.getAttribute("data-day")
                  if dayName != null && dayName.nonEmpty then
                    currentDayVar.set(Some(dayName))
              }
            },
            dayObserverOptions
          )
          
          // Time observer - tracks which time slot is at the top
          val timeObserverOptions = js.Dynamic.literal(
            rootMargin = s"-${headerHeight.toInt + 40}px 0px -85% 0px",
            threshold = 0.0
          ).asInstanceOf[IntersectionObserverInit]
          
          val timeObserver = new IntersectionObserver(
            { (entries, _) =>
              entries.foreach { entry =>
                if entry.isIntersecting then
                  val timeText = entry.target.getAttribute("data-time")
                  if timeText != null && timeText.nonEmpty then
                    currentTimeVar.set(Some(timeText))
              }
            },
            timeObserverOptions
          )
          
          // Observe all day sections and time slots after a small delay to ensure DOM is ready
          dom.window.setTimeout(
            () => {
              contentEl.querySelectorAll("[data-day]").foreach { el =>
                dayObserver.observe(el.asInstanceOf[dom.Element])
              }
              contentEl.querySelectorAll("[data-time]").foreach { el =>
                timeObserver.observe(el.asInstanceOf[dom.Element])
              }
              
              // Initialize with first visible day/time
              val firstDay = contentEl.querySelector("[data-day]")
              if firstDay != null then
                val dayName = firstDay.getAttribute("data-day")
                if dayName != null then currentDayVar.set(Some(dayName))
              
              val firstTime = contentEl.querySelector("[data-time]")
              if firstTime != null then
                val timeText = firstTime.getAttribute("data-time")
                if timeText != null then currentTimeVar.set(Some(timeText))
            },
            100
          )
          
        },
        onUnmountCallback { _ =>
          // Note: observers will be garbage collected when the element unmounts
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
                  dataAttr("time") := "Evening Lightning Talks",
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
