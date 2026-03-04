package co.wtf.openspaces.components.schedule

import com.raquo.laminar.api.L.{*, given}
import neotype.unwrap
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import co.wtf.openspaces.{Room, RoomSlot}
import co.wtf.openspaces.discussions.{Discussion, DiscussionState, VotePosition}
import co.wtf.openspaces.activities.{Activity, ActivityState}

/** Desktop-oriented calendar day view showing schedule as a grid with rooms as columns.
  * Read-only view that surfaces voter details for each discussion.
  */
object CalendarDayView:
  private val dayFormat = DateTimeFormatter.ofPattern("EEEE, MMMM d")
  private val timeFormat = DateTimeFormatter.ofPattern("h:mm a")

  def apply(
    $discussionState: Signal[DiscussionState],
    $activityState: Signal[ActivityState],
  ): HtmlElement =
    // Track selected day
    val selectedDay = Var[Option[LocalDate]](None)

    // Derive available days from discussion state
    val $availableDays: Signal[List[LocalDate]] =
      $discussionState.map(_.slots.map(_.date).sorted)

    // Initialize selected day when days become available
    val $initDay = $availableDays.map(_.headOption)

    div(
      cls := "CalendarDayView",
      // Initialize selected day from first available
      $initDay --> { dayOpt =>
        if selectedDay.now().isEmpty then
          selectedDay.set(dayOpt)
      },
      // Day selector header
      div(
        cls := "CalendarDayView-header",
        children <-- $availableDays.map { days =>
          days.map { day =>
            button(
              cls := "CalendarDayView-dayButton",
              cls <-- selectedDay.signal.map { selected =>
                if selected.contains(day) then "CalendarDayView-dayButton--active" else ""
              },
              onClick --> { _ => selectedDay.set(Some(day)) },
              day.format(dayFormat),
            )
          }
        },
      ),
      // Debug info
      child <-- Signal.combine($discussionState, selectedDay.signal).map {
        case (state, dayOpt) =>
          val totalDiscussions = state.data.size
          val scheduled = state.data.values.count(_.roomSlot.isDefined)
          div(
            cls := "CalendarDayView-debug",
            s"Debug: $totalDiscussions discussions total, $scheduled scheduled. Days available: ${state.slots.map(_.date).mkString(", ")}. Selected: ${dayOpt.getOrElse("none")}"
          )
      },
      // Grid container
      child <-- Signal.combine($discussionState, $activityState, selectedDay.signal).map {
        case (state, activityState, Some(day)) =>
          renderDayGrid(state, activityState, day)
        case _ =>
          div(cls := "CalendarDayView-empty", "Select a day to view the schedule")
      },
    )

  private def renderDayGrid(
    state: DiscussionState,
    activityState: ActivityState,
    day: LocalDate,
  ): HtmlElement =
    // Get slots for this day
    val daySlots = state.slots.find(_.date == day)
    val timeSlots = daySlots.map(_.slots).getOrElse(List.empty)
    val rooms = timeSlots.headOption.map(_.rooms).getOrElse(Room.all)

    // Get activities for this day
    val dayActivities = activityState.activities.values
      .filter(_.eventTime.toLocalDate == day)
      .toList
      .sortBy(_.eventTime)

    div(
      cls := "CalendarDayView-grid",
      // Room headers row
      div(
        cls := "CalendarDayView-row CalendarDayView-row--header",
        div(cls := "CalendarDayView-timeCell CalendarDayView-timeCell--header", "Time"),
        rooms.map { room =>
          div(
            cls := "CalendarDayView-roomHeader",
            div(cls := "CalendarDayView-roomName", room.name),
            div(cls := "CalendarDayView-roomCapacity", s"capacity: ${room.capacity}"),
          )
        },
      ),
      // Time slot rows
      timeSlots.map { slotForAllRooms =>
        val timeSlot = slotForAllRooms.time
        div(
          cls := "CalendarDayView-row",
          div(
            cls := "CalendarDayView-timeCell",
            div(cls := "CalendarDayView-timeStart", timeSlot.startTime.format(timeFormat)),
            div(cls := "CalendarDayView-timeEnd", timeSlot.endTime.format(timeFormat)),
          ),
          rooms.map { room =>
            val roomSlot = RoomSlot(room, timeSlot)
            val discussion = state.roomSlotContent(roomSlot)
            renderCell(discussion)
          },
        )
      },
      // Activities section (if any)
      Option.when(dayActivities.nonEmpty)(
        div(
          cls := "CalendarDayView-activitiesSection",
          div(cls := "CalendarDayView-activitiesHeader", "Activities"),
          dayActivities.map(renderActivity),
        )
      ),
    )

  private def renderCell(discussion: Option[Discussion]): HtmlElement =
    discussion match
      case Some(d) =>
        val interestedVoters = d.interestedParties
          .filter(_.position == VotePosition.Interested)
          .map(_.voter.unwrap)
          .toList
          .sorted

        div(
          cls := "CalendarDayView-cell CalendarDayView-cell--filled",
          div(cls := "CalendarDayView-topic", d.topicName),
          div(cls := "CalendarDayView-facilitator", s"by ${d.facilitatorName}"),
          div(
            cls := "CalendarDayView-votes",
            span(cls := "CalendarDayView-voteCount", s"${d.votes} interested"),
          ),
          div(
            cls := "CalendarDayView-voters",
            interestedVoters.map { voter =>
              span(cls := "CalendarDayView-voter", voter)
            },
          ),
        )
      case None =>
        div(cls := "CalendarDayView-cell CalendarDayView-cell--empty", "Open")

  private def renderActivity(activity: Activity): HtmlElement =
    val memberNames = activity.members.map(_.person.unwrap).sorted
    div(
      cls := "CalendarDayView-activity",
      div(
        cls := "CalendarDayView-activityTime",
        activity.eventTime.format(timeFormat),
      ),
      div(
        cls := "CalendarDayView-activityContent",
        div(cls := "CalendarDayView-activityDescription", activity.descriptionText),
        div(cls := "CalendarDayView-activityCreator", s"by ${activity.creatorName}"),
        div(
          cls := "CalendarDayView-activityMembers",
          s"${activity.interestCount} interested",
          Option.when(memberNames.nonEmpty)(
            span(cls := "CalendarDayView-memberList", s": ${memberNames.mkString(", ")}")
          ),
        ),
      ),
    )
