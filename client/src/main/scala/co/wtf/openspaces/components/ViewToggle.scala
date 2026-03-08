package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import scala.scalajs.js
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import co.wtf.openspaces.{AppState, SvgIcon, GlyphiconUtils}
import co.wtf.openspaces.discussions.DiscussionState
import co.wtf.openspaces.activities.{Activity, ActivityState}

/** Unified bottom navigation bar.
  * Fixed to bottom of screen with:
  * - Action buttons (Jump to Now, Add Activity)
  * - Navigation tabs (Topics, Activities, Hack, Schedule, Admin)
  * - Badge on Topics tab for unvoted count
  */
object ViewToggle:
  
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
      case (Some((slotTime, slotDomId)), Some((actTime, actDomId))) =>
        if actTime.isBefore(slotTime) then Some(actDomId) else Some(slotDomId)
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

  /** Scroll to a specific element by ID */
  private def scrollToElement(targetId: String): Unit =
    Option(dom.document.getElementById(targetId)).foreach { element =>
      element.asInstanceOf[js.Dynamic].scrollIntoView(
        js.Dynamic.literal(behavior = "smooth", block = "center")
      )
    }

  /** Scroll to the next upcoming schedule item */
  def scrollToNextItem(state: DiscussionState, activities: List[Activity]): Unit =
    findNextUpcomingId(state, activities)
      .orElse(findFirstRenderedId(state, activities))
      .foreach(scrollToElement)

  /** Scroll to a specific activity by its event time */
  def scrollToActivity(eventTime: LocalDateTime): Unit =
    scrollToElement(activitySlotId(eventTime))

  def apply(
    currentView: Var[AppView],
    adminModeEnabled: Signal[Boolean],
  ) =
    val unjudgedTopicCount = AppState.unjudgedTopicCount
    val unjudgedActivityCount = AppState.unjudgedActivityCount
    val discussionState = AppState.discussionState
    val activityState = AppState.activityState

    def jumpToNow(): Unit =
      // Switch to schedule view if not there, then scroll to now
      if currentView.now() != AppView.Schedule then
        AppState.navigateTo(AppView.Schedule)
        // Small delay to let view render before scrolling
        dom.window.setTimeout(
          () => scrollToNextItem(
            discussionState.now(),
            activityState.now().activities.values.toList
          ),
          100
        )
      else
        scrollToNextItem(
          discussionState.now(),
          activityState.now().activities.values.toList
        )

    div(
      cls := "BottomNav",
      // Single row of 5 buttons: Topics | Activities | + | Hack | Schedule
      // Admin button shown in header when enabled, not in nav
      button(
        cls := "BottomNav-tab",
        cls <-- currentView.signal.map { view =>
          if view == AppView.Topics then "BottomNav-tab--active" else ""
        },
        onClick --> Observer(_ => AppState.navigateTo(AppView.Topics)),
        span(
          cls := "BottomNav-iconWrapper",
          span(cls := "BottomNav-icon", "💬"),
          child.maybe <-- unjudgedTopicCount.map { count =>
            Option.when(count > 0)(
              span(
                cls := "BottomNav-badge",
                if count > 99 then "99+" else count.toString,
              )
            )
          },
        ),
        span(cls := "BottomNav-label", "Topics"),
      ),
      button(
        cls := "BottomNav-tab",
        cls <-- currentView.signal.map { view =>
          if view == AppView.LightningTalks then "BottomNav-tab--active" else ""
        },
        onClick --> Observer(_ => AppState.navigateTo(AppView.LightningTalks)),
        span(
          cls := "BottomNav-iconWrapper",
          span(cls := "BottomNav-icon", "🎤"),
          child.maybe <-- unjudgedActivityCount.map { count =>
            Option.when(count > 0)(
              span(
                cls := "BottomNav-badge",
                if count > 99 then "99+" else count.toString,
              )
            )
          },
        ),
        span(cls := "BottomNav-label", "Activities"),
      ),
      // Center plus button for creating new entities
      button(
        cls := "BottomNav-tab BottomNav-tab--create",
        onClick --> Observer(_ => AppState.openCreateSheet()),
        span(cls := "BottomNav-icon BottomNav-icon--create", "+"),
      ),
      button(
        cls := "BottomNav-tab",
        cls <-- currentView.signal.map { view =>
          if view == AppView.Hackathon then "BottomNav-tab--active" else ""
        },
        onClick --> Observer(_ => AppState.navigateTo(AppView.Hackathon)),
        span(cls := "BottomNav-icon", "🛠"),
        span(cls := "BottomNav-label", "Hack"),
      ),
      button(
        cls := "BottomNav-tab",
        cls <-- currentView.signal.map { view =>
          if view == AppView.Schedule then "BottomNav-tab--active" else ""
        },
        onClick --> Observer(_ => jumpToNow()),
        span(cls := "BottomNav-icon", "📅"),
        span(cls := "BottomNav-label", "Schedule"),
      ),
    )
