package co.wtf.openspaces

import animus.*
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import org.scalajs.dom.window
import zio.json.*
import neotype.*

val localStorage = window.localStorage

import org.scalajs.dom
import scala.scalajs.js.URIUtils

/** Tracks active swap animations for smooth position transitions.
  *
  * Animation flow:
  *   1. When a swap is confirmed, we set initial offsets (from old
  *      position to new) 2. After a brief delay (one animation
  *      frame), we clear the offsets to (0, 0) 3. The spring
  *      animation interpolates from the offset to (0, 0), creating
  *      the sliding effect
  */
object SwapAnimationState:
  // Stores the INITIAL offsets for each topic (where they should appear to come FROM)
  val initialOffsets: Var[Map[TopicId, (Double, Double)]] = Var(
    Map.empty,
  )

  // Stores whether each topic's animation should be "animating to zero"
  val animatingToZero: Var[Set[TopicId]] = Var(Set.empty)

  /** Spring animation typically needs ~600-800ms to settle */
  val cleanupDelayMs: Int = 1000

  /** Starts a swap animation between two topics based on their
    * RoomSlots
    */
  def startAnimation(
    topic1: TopicId,
    slot1: RoomSlot,
    topic2: TopicId,
    slot2: RoomSlot,
  ): Unit =
    // Calculate pixel offsets based on grid layout
    // The schedule grid has 4 rooms (columns) and multiple time slots (rows)
    val cellWidth = 60.0 // Approximate cell width in pixels
    val cellHeight =
      64.0 // Approximate cell height (8vh on typical mobile)

    // Calculate column difference (rooms are columns 0-3)
    val col1 = slot1.room.id
    val col2 = slot2.room.id
    val colDiff = col2 - col1

    // Calculate row difference based on time slots
    val row1 = timeSlotToRow(slot1.timeSlot)
    val row2 = timeSlotToRow(slot2.timeSlot)
    val rowDiff = row2 - row1

    val xOffset = colDiff * cellWidth
    val yOffset = rowDiff * cellHeight

    // After swap, topic1 is now at slot1 (rendered there in DOM)
    // But topic1 WAS at slot2 before the swap
    // Initial offset for topic1 = position difference (slot2 - slot1)
    val topic1Offset = (xOffset, yOffset)
    val topic2Offset = (-xOffset, -yOffset)

    // Step 1: Set initial offsets (topics appear at their OLD positions)
    initialOffsets.update(
      _ + (topic1 -> topic1Offset) + (topic2 -> topic2Offset),
    )

    // Step 2: After a brief delay (to allow DOM to update), trigger animation to zero
    // We use a tiny delay to ensure the component has rendered with the initial offset
    window.setTimeout(
      () => animatingToZero.update(_ + topic1 + topic2),
      16, // ~1 animation frame (16ms)
    )

    // Step 3: Clean up after animation completes
    window.setTimeout(
      () => {
        initialOffsets.update(_ - topic1 - topic2)
        animatingToZero.update(_ - topic1 - topic2)
      },
      cleanupDelayMs,
    )

  /** Gets the animated offset signal for a topic. Returns the initial
    * offset if not yet animating, or (0,0) if animating to final
    * position. The spring will interpolate between these values.
    */
  def getOffsetSignal(
    topicId: TopicId,
  ): Signal[(Double, Double)] =
    initialOffsets.signal.combineWith(animatingToZero.signal).map {
      case (offsets, animating) =>
        if (animating.contains(topicId)) {
          // Target position: animate TO (0, 0)
          (0.0, 0.0)
        }
        else {
          // Initial position: offset from old position
          offsets.getOrElse(topicId, (0.0, 0.0))
        }
    }

  /** Helper to convert TimeSlot to a row index for offset calculation
    */
  private def timeSlotToRow(
    timeSlot: TimeSlot,
  ): Int =
    val startTime = timeSlot.startTime
    val dayOffset = startTime.getDayOfYear * 100
    val timeOffset = startTime.getHour * 60 + startTime.getMinute
    (dayOffset + timeOffset) / 50

/** Centralized menu positioning - uses viewport dimensions only */
object MenuPositioning:
  private val menuMargin = 10.0
  private val standardMenuMaxWidth = 350.0

  /** Standard positioning for dropdown menus - centered horizontally,
    * near top of screen
    */
  def standardMenuPosition(): (Double, Double) =
    val viewportHeight = dom.window.innerHeight
    val viewportWidth = dom.window.innerWidth

    val menuWidth =
      Math.min(standardMenuMaxWidth, viewportWidth - 2 * menuMargin)

    // Center the menu horizontally
    val x = (viewportWidth - menuWidth) / 2

    // Position near top of screen to ensure visibility
    val y = Math.max(20.0, viewportHeight * 0.05)

    (x, y)

object FrontEnd extends App:
  lazy val container = dom.document.getElementById("app")

  val discussionState: Var[DiscussionState] =
    Var(
      DiscussionState(DiscussionState.timeSlotExamples, Map.empty),
    ) //

  val errorBanner =
    ErrorBanner()

  val submitNewTopic: Observer[DiscussionAction] = Observer {
    case discussion @ (add: DiscussionAction.Add) =>
      if (add.facilitator.unwrap.trim.length < 2)
        errorBanner.error.set(
          Some("User name too short. Tell us who you are!"),
        )
      else
        errorBanner.error.set(None)
        topicUpdates.sendOne(discussion)
    case _ => ()
  }

  val name = getOrCreatePersistedName()
  def liveTopicSubmissionAndVoting(
    updateTargetDiscussion: Observer[Discussion],
  ) =
    div(
      TopicSubmission(submitNewTopic,
                      name.signal,
                      errorBanner.error.toObserver,
      ),
      DiscussionSubview(
        discussionState.signal.map(_.data.values.toList),
        None,
        name.signal,
        topicUpdates.sendOne,
        updateTargetDiscussion,
      ),
    )

  val activeDiscussion: Var[Option[Discussion]] =
    Var(None)

  // Popover state
  val popoverState: Var[Option[Discussion]] =
    Var(None)

  // Swap action menu state: (selected Discussion, target Discussion)
  val swapMenuState: Var[Option[(Discussion, Discussion)]] =
    Var(None)

  // Unscheduled discussions menu state
  val unscheduledMenuState: Var[Option[RoomSlot]] =
    Var(None)

  // Active discussion actions menu state
  val activeDiscussionMenuState: Var[Option[Discussion]] =
    Var(None)

  val updateTargetDiscussion: Observer[Discussion] =
    Observer[Discussion] { discussion =>
      dom.document
        .getElementsByClassName("ActiveDiscussion")
        .head
        .scrollIntoView(
          top = false,
        )
      activeDiscussion.set(Some(discussion))
    }

  val setActiveDiscussion: Observer[Discussion] = Observer {
    discussion =>
      discussion.roomSlot match
        case Some(value) =>
          topicUpdates.sendOne(
            DiscussionAction.UpdateRoomSlot(discussion.id, value),
          )
        case None =>
          topicUpdates.sendOne(
            DiscussionAction.Unschedule(discussion.id),
          )

      activeDiscussion.set(Some(discussion))
  }

  val logoutButton = div(
    button(
      onClick --> Observer { _ =>
        deleteCookie("access_token")
        window.location.reload()
      },
      "Logout",
    ),
  )

  val dismissSwapMenu: Observer[Unit] =
    Observer { _ =>
      swapMenuState.set(None)
    }

  val dismissUnscheduledMenu: Observer[Unit] =
    Observer { _ =>
      unscheduledMenuState.set(None)
    }

  val dismissActiveDiscussionMenu: Observer[Unit] =
    Observer { _ =>
      activeDiscussionMenuState.set(None)
    }

  val app =
    div(
      cls := "PageContainer",
      topicUpdates.connect,
      // Popover component at top level
      // Swap action menu at top level
      child <-- swapMenuState.signal.map {
        case Some((selectedDiscussion, targetDiscussion)) =>
          Menu(
            selectedDiscussion,
            targetDiscussion,
            topicUpdates.sendOne,
            dismissSwapMenu,
          )
        case None =>
          div()
      },
      // Unscheduled discussions menu at top level
      child <-- unscheduledMenuState.signal
        .combineWith(discussionState.signal)
        .map {
          case (Some(roomSlot), discussionState) =>
            val unscheduledDiscussions = discussionState.data.values
              .filter(_.roomSlot.isEmpty)
              .toList
            UnscheduledDiscussionsMenu(
              unscheduledDiscussions,
              roomSlot,
              name.signal,
              topicUpdates.sendOne,
              dismissUnscheduledMenu,
              setActiveDiscussion,
            )
          case _ =>
            div()
        },
      // Active discussion action menu at top level
      child <-- activeDiscussionMenuState.signal.map {
        case Some(discussion) =>
          ActiveDiscussionActionMenu(
            discussion,
            topicUpdates.sendOne,
            dismissActiveDiscussionMenu,
          )
        case None =>
          div()
      },
      getCookie("access_token") match {
        case Some(accessToken) =>
          div(
            logoutButton,
            ticketCenter(topicUpdates),
            topicUpdates.received --> Observer {
              (event: DiscussionActionConfirmed) =>
                // Handle rejection feedback
                event match
                  case DiscussionActionConfirmed.Rejected(
                        _: DiscussionAction.SwapTopics,
                      ) =>
                    errorBanner.error.set(
                      Some(
                        "Swap failed: One or both topics were moved by another user. Please try again.",
                      ),
                    )
                  case DiscussionActionConfirmed.Rejected(
                        _: DiscussionAction.MoveTopic,
                      ) =>
                    errorBanner.error.set(
                      Some(
                        "Move failed: That slot was just filled by another user. Please try again.",
                      ),
                    )
                  // Trigger swap animation before state update
                  case DiscussionActionConfirmed.SwapTopics(
                        topic1,
                        newSlot1,
                        topic2,
                        newSlot2,
                      ) =>
                    // Start the swap animation - newSlot1 is where topic1 is going TO
                    // So topic1 was at newSlot2 (they swapped), and topic2 was at newSlot1
                    SwapAnimationState.startAnimation(
                      topic1,
                      newSlot1, // topic1's new position
                      topic2,
                      newSlot2, // topic2's new position
                    )
                  case _ => ()

                discussionState
                  .update { existing =>
                    val state = existing(event)
                    val (topicId, shouldClearActive) =
                      handleDiscussionActionConfirmed(event)

                    if (shouldClearActive) {
                      activeDiscussion.set(None)
                    }

                    topicId.foreach { id =>
                      if (
                        activeDiscussion.now().map(_.id).contains(id)
                      ) {
                        activeDiscussion.set(state.data.get(id))
                      }
                    }

                    state
                  }
            },
            errorBanner.component,
            NameBadge(name),
            ScheduleView(
              discussionState,
              activeDiscussion,
              topicUpdates.sendOne,
              name.signal,
              setActiveDiscussion,
              popoverState,
              swapMenuState,
              unscheduledMenuState,
              activeDiscussionMenuState,
            ),
            liveTopicSubmissionAndVoting(updateTargetDiscussion),
          )
        case None =>
          div(
            span("No access token found. Please log in."),
            a(
              href := "/auth",
              "Login",
            ),
          )

      },
    )

  render(container, app)

def getCookie(
  name: String,
): Option[String] = {
  val cookieString = dom.document.cookie
  val cookies = cookieString.split(";")

  cookies.find(_.trim.startsWith(s"$name=")) match {
    case Some(cookie) =>
      val encodedValue = cookie.trim.substring(name.length + 1)
      Some(URIUtils.decodeURIComponent(encodedValue))
    case None => None
  }
}

private def getOrCreatePersistedName(): Var[Person] =
  val name =
    try {
      val retrieved =
        localStorage
          .getItem("name")
      Option.when(retrieved != null && !retrieved.isBlank)(
        Person(retrieved),
      )
    }
    catch {
      case e: Exception =>
        None
    }
  Var(name.getOrElse(Person("")))

private def BannerLogo() =
  div(width := "100%",
      img(cls := "LogoImg",
          src := "./wtf-web-nodate.jpg",
          role := "img",
      ),
  )

private def NameBadge(
  textVar: Var[Person],
) =
  div(
    cls := "Banner",
    img(cls := "LogoImg",
        src := "./wtf-web-nodate.jpg",
        role := "img",
    ),
    div(
      span("Name:"),
      input(
        placeholder := "Enter your name",
        value <-- textVar.signal.map(_.unwrap),
        onInput.mapToValue.map(Person(_)) --> textVar,
        textVar --> Observer { (value: Person) =>
          localStorage.setItem("name", value.unwrap)
        },
      ),
    ),
  )

private def TopicSubmission(
  submitEffect: Observer[DiscussionAction],
  name: StrictSignal[Person],
  setErrorMsg: Observer[Option[String]],
) =
  val textVar = Var("")
  div(
    cls := "Flex",
    span(
      textArea(
        fontFamily := "Roboto",
        placeholder := "Create a topic...",
        value <-- textVar,
        onInput.mapToValue --> textVar,
      ),
    ),
    button(
      onClick
        .mapTo(textVar.now())
        .map(s =>
          val res = Topic.make(s)
          res match
            case Left(value) =>
              setErrorMsg.onNext(Some(value))
              None
            case Right(value) =>
              Some(value),
        )
        .filter(_.isDefined)
        .map(_.get)
        .map(topicTitle =>
          DiscussionAction.Add(
            topicTitle,
            name.now(),
          ),
        )
        .tapEach { case _ =>
          textVar.set("")
        } --> submitEffect,
      "Submit",
    ),
  )

private def SingleDiscussionComponent(
  name: StrictSignal[Person],
  topicUpdates: DiscussionAction => Unit,
  updateTargetDiscussion: Observer[Discussion],
  signal: Signal[Option[Discussion]],
  transition: Option[Transition],
  iconModifiers: Seq[Modifier[HtmlElement]] = Seq.empty,
) = {
  signal.map {
    case Some(topic) =>
      val currentFeedback =
        topic.interestedParties.find(_.voter == name.now())
      val backgroundColorByPosition = "#C6DAD7"

      val $characters: List[(String, Int)] =
        topic.topic.unwrap.split("").zipWithIndex.toList

      val isInterested =
        currentFeedback.exists(_.position == VotePosition.Interested)
      val isNotInterested =
        currentFeedback.exists(
          _.position == VotePosition.NotInterested,
        )
      def handleVote(
        target: Option[VotePosition],
      ) =
        val voter = name.now()
        topicUpdates(
          DiscussionAction.RemoveVote(topic.id, voter),
        )
        target.foreach { position =>
          topicUpdates(
            DiscussionAction.Vote(
              topic.id,
              Feedback(voter, position),
            ),
          )
        }

      // import neotype.unwrap
      div(
        cls := "TopicCard", // TODO Make this a component than can be used in the schedule view!
        backgroundColor := backgroundColorByPosition,
        transition match
          case Some(value) => value.height
          case None        => height("15vh")
        ,
        div(
          cls := "MainActive",
          div(topic.topicName)

//                children <-- $characters.splitTransition(identity) {
//                  case (_, (character, _), _, transition) =>
//                    val newCharacter = character match
//                      case " " => '\u00A0'
//                      case _ => character.charAt(0)
//                    div(
//                      newCharacter,
//                      display.inlineFlex,
//                      transition.width,
//                      //                              transition.height
//                    )
//                }
          ,
          if (
            List("bill", "emma").exists(admin =>
              name.now().unwrap.toLowerCase().contains(admin),
            )
          )
            button(
              cls := "delete-topic",
              color := "red",
              border := "none",
              backgroundColor := "transparent",
              onClick --> Observer { _ =>
                // TODO Make sure this updates the ActiveDiscussion, so it's not left lingering on the schedule.
                topicUpdates(DiscussionAction.Delete(topic.id))
              },
              "x",
            )
          else span(),
        ),
        span(
          cls := "SecondaryActive",
          span(
            SvgIcon(topic.glyphicon).amend(iconModifiers*),
            span(topic.facilitatorName),
            topic.roomSlot match {
              case Some(roomSlot) =>
                span(
                  roomSlot.displayString,
                )
              case None =>
                span("Unscheduled")
            },
          ),
        ),
        div(
          cls := "VoteColumn",
          div(
            cls := "VoteChevronStack",
            button(
              cls := (
                if isInterested then
                  "AddButton VoteButton VoteButton--active"
                else "AddButton VoteButton"
              ),
              onClick --> Observer { _ =>
                handleVote(
                  if isInterested then None
                  else Some(VotePosition.Interested),
                )
              },
              SvgIcon(GlyphiconUtils.chevronUp),
            ),
            span(
              cls := "VoteCount",
              topic.votes.toString,
            ),
            button(
              cls := (
                if isNotInterested then
                  "AddButton VoteButton VoteButton--active"
                else "AddButton VoteButton"
              ),
              onClick --> Observer { _ =>
                handleVote(
                  if isNotInterested then None
                  else Some(VotePosition.NotInterested),
                )
              },
              SvgIcon(GlyphiconUtils.chevronDown),
            ),
          ),
        ),
      )
    case None =>
      div("nothing")
  }

}

private def DiscussionSubview(
  topicsOfInterest: Signal[List[Discussion]],
  votePosition: Option[VotePosition],
  name: StrictSignal[Person],
  topicUpdates: DiscussionAction => Unit,
  updateTargetDiscussion: Observer[Discussion],
) =
  div(
    cls := "TopicsContainer",
    children <--
      topicsOfInterest
        .splitTransition(_.id)(
          (
            index: TopicId,
            topic: Discussion,
            signal: Signal[Discussion],
            transition: Transition,
          ) =>
            div(
              child <-- SingleDiscussionComponent(
                name,
                topicUpdates,
                updateTargetDiscussion,
                signal.map(Some(_)),
                Some(transition),
              ),
            ),
        ),
  )

enum AppView:
  case Home
  case ScheduleView
  case SubmitTopic

def ScheduleSlotComponent(
  timeSlot: TimeSlot,
  room: Room,
  $discussionState: Signal[DiscussionState],
  updateDiscussion: Observer[Discussion],
  $activeDiscussion: StrictSignal[Option[Discussion]],
  showPopover: Observer[Discussion],
  showSwapMenu: Observer[(Discussion, Discussion)],
  showUnscheduledMenu: Observer[RoomSlot],
  topicUpdates: DiscussionAction => Unit,
) =
  span(
    child <-- $discussionState.map { discussionState =>
      span(
        child <-- $activeDiscussion.map { discussionO =>
          discussionState.roomSlotContent(
            RoomSlot(room, timeSlot),
          ) match
            case Some(value) =>
              val selectedTopicStyling =
                if (
                  $activeDiscussion.now().map(_.id).contains(value.id)
                )
                  "activeTopicIcon"
                else ""
              // Check if there's an active discussion that's different from this slot's topic
              val isSwappable = discussionO.exists(active =>
                active.id != value.id && active.roomSlot.isDefined,
              )

              // Get the animated offset for this topic (if it's part of a swap)
              val $offset =
                SwapAnimationState.getOffsetSignal(value.id)
              // Use spring animation for smooth movement
              val $animatedX = $offset.map(_._1).spring
              val $animatedY = $offset.map(_._2).spring

              span(
                cls := "swap-topic-icon",
                // Apply animated transform based on swap offset
                transform <-- $animatedX.combineWith($animatedY).map {
                  case (x, y) =>
                    if (x != 0.0 || y != 0.0)
                      s"translate(${x}px, ${y}px)"
                    else "none"
                },
                onClick.stopPropagation.mapTo(value) --> showPopover,
                // Long-press to show swap menu when there's an active discussion
                if (isSwappable)
                  onContextMenu.preventDefault --> Observer {
                    (event: org.scalajs.dom.MouseEvent) =>
                      event.stopPropagation()
                      discussionO.foreach { activeDiscussion =>
                        showSwapMenu.onNext((activeDiscussion, value))
                      }
                  }
                else emptyMod,
                onClick.mapTo(
                  value,
                ) --> updateDiscussion, // TODO This is causing an unecesary update to be sent to server
                SvgIcon(value.glyphicon,
                        s"filledTopic $selectedTopicStyling",
                ),
              )
            case None =>
              discussionO match
                case Some(discussion) =>
                  discussion.roomSlot match
                    case Some(value)
                        if RoomSlot(room,
                                    timeSlot,
                        ) == value => // TODO Make this impossible
                      SvgIcon(discussion.glyphicon, "filledTopic")
                    case Some(_) =>
                      // Empty slot when active discussion is scheduled elsewhere
                      // Quick click shows unscheduled menu, long-press moves the topic here
                      span(
                        cls := "emptySlotWithActiveDiscussion",
                        SvgIcon(GlyphiconUtils.emptySlot),
                        onClick.stopPropagation.mapTo(
                          RoomSlot(room, timeSlot),
                        ) --> showUnscheduledMenu,
                        onContextMenu.preventDefault --> Observer {
                          (event: org.scalajs.dom.MouseEvent) =>
                            event.stopPropagation()
                            topicUpdates(
                              DiscussionAction.MoveTopic(
                                discussion.id,
                                RoomSlot(room, timeSlot),
                              ),
                            )
                        },
                      )
                    case None =>
                      span(
                        SvgIcon(GlyphiconUtils.plus),
                        onClick.mapTo(
                          discussion.copy(roomSlot =
                            Some(RoomSlot(room, timeSlot)),
                          ),
                        ) --> updateDiscussion,
                      )
                case None =>
                  // Empty slot - show menu on short click, long press logs to console (placeholder)
                  span(
                    SvgIcon(GlyphiconUtils.emptySlot),
                    onClick.stopPropagation.mapTo(
                      RoomSlot(room, timeSlot),
                    ) --> showUnscheduledMenu,
                    onContextMenu.preventDefault --> Observer {
                      (event: org.scalajs.dom.MouseEvent) =>
                        event.stopPropagation()
                        // Placeholder for future behavior when no discussion is selected
                        dom.console.log(
                          "Long press on empty slot (no discussion selected) - placeholder for future behavior",
                        )
                    },
                  )
        },
      )
    },
  )

def SlotSchedule(
  $discussionState: Signal[DiscussionState],
  $timeSlotsForAllRooms: Signal[TimeSlotForAllRooms],
  updateDiscussion: Observer[Discussion],
  activeDiscussion: StrictSignal[Option[Discussion]],
  showPopover: Observer[Discussion],
  showSwapMenu: Observer[(Discussion, Discussion)],
  showUnscheduledMenu: Observer[RoomSlot],
  topicUpdates: DiscussionAction => Unit,
) =
  div(
    child <--
      $timeSlotsForAllRooms.map { timeSlotsForAllRooms =>
        div(
          cls := "SlotRow",
          div(cls := "TimeOfSlot", timeSlotsForAllRooms.time.s),
          timeSlotsForAllRooms.rooms
            .map { room =>
              div(
                cls := "Cell",
                ScheduleSlotComponent(timeSlotsForAllRooms.time,
                                      room,
                                      $discussionState,
                                      updateDiscussion,
                                      activeDiscussion,
                                      showPopover,
                                      showSwapMenu,
                                      showUnscheduledMenu,
                                      topicUpdates,
                ),
              )
            },
        )
      },
  )

case class ErrorBanner(
  error: Var[Option[String]] = Var(None)):
  val component =
    div(
      child <--
        error.signal.map {
          case Some(value) =>
            div(
              cls := "ErrorBanner",
              span(cls := "ErrorBanner-message", "Error: " + value),
              button(
                cls := "ErrorBanner-dismiss",
                onClick --> Observer(_ => error.set(None)),
                "×",
              ),
            )
          case None =>
            div()
        },
    )

def Menu(
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
        SvgIcon(selectedDiscussion.glyphicon),
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
        SvgIcon(targetDiscussion.glyphicon),
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

def UnscheduledDiscussionsMenu(
  unscheduledDiscussions: List[Discussion],
  targetRoomSlot: RoomSlot,
  facilitator: StrictSignal[Person],
  topicUpdates: DiscussionAction => Unit,
  dismissMenu: Observer[Unit],
  setActiveDiscussion: Observer[Discussion],
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
    div(
      cls := "Menu-section",
      div(cls := "Menu-label", "Add New Discussion:"),
      textArea(
        cls := "Menu-textArea",
        placeholder := "Describe the discussion to schedule...",
        value <-- textVar.signal,
        onInput.mapToValue --> textVar,
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
      div(cls := "Menu-label", "Available Discussions:"),
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
                  DiscussionAction.UpdateRoomSlot(
                    discussion.id,
                    targetRoomSlot,
                  ),
                )
                // Set the discussion as active after assigning it to the room slot
                setActiveDiscussion.onNext(
                  discussion.copy(roomSlot = Some(targetRoomSlot)),
                )
                dismissMenu.onNext(())
              },
              SvgIcon(discussion.glyphicon),
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

def ActiveDiscussionActionMenu(
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
              DiscussionAction.Unschedule(discussion.id),
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
      SvgIcon(discussion.glyphicon),
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

private def activeDiscussionLongPressBinder(
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

def ScheduleView(
  fullSchedule: Var[DiscussionState],
  activeDiscussion: Var[Option[Discussion]],
  topicUpdates: DiscussionAction => Unit,
  name: StrictSignal[Person],
  updateTargetDiscussion: Observer[Discussion],
  popoverState: Var[Option[Discussion]],
  swapMenuState: Var[Option[(Discussion, Discussion)]],
  unscheduledMenuState: Var[Option[RoomSlot]],
  activeDiscussionMenuState: Var[Option[Discussion]],
) =
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
        child <-- SingleDiscussionComponent(
          name,
          topicUpdates,
          updateTargetDiscussion,
          activeDiscussion.signal,
          None,
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
        SlotSchedules(
          fullSchedule.signal,
          updateTargetDiscussion,
          activeDiscussion.signal,
          showPopover,
          showSwapMenu,
          showUnscheduledMenu,
          topicUpdates,
        ),
      ),
    ),
  )

def SlotSchedules(
  $discussionState: Signal[DiscussionState],
  updateDiscussion: Observer[Discussion],
  activeDiscussion: StrictSignal[Option[Discussion]],
  showPopover: Observer[Discussion],
  showSwapMenu: Observer[(Discussion, Discussion)],
  showUnscheduledMenu: Observer[RoomSlot],
  topicUpdates: DiscussionAction => Unit,
) =
  div(
    children <--
      $discussionState.map(discussionState =>
        discussionState.slots.map(daySlot =>
          div(
            div(daySlot.date.getDayOfWeek.toString().take(3)),
            daySlot.slots.map(timeSlotsForAllRooms =>
              div(
                cls := "SlotRow",
                div(cls := "TimeOfSlot", timeSlotsForAllRooms.time.s),
                timeSlotsForAllRooms.rooms
                  .map { room =>
                    div(
                      cls := "Cell",
                      ScheduleSlotComponent(timeSlotsForAllRooms.time,
                                            room,
                                            $discussionState,
                                            updateDiscussion,
                                            activeDiscussion,
                                            showPopover,
                                            showSwapMenu,
                                            showUnscheduledMenu,
                                            topicUpdates,
                      ),
                    )
                  },
              ),
            ),
          ),
        ),
      ),
  )

def deleteCookie(
  name: String,
) =
  dom.document.cookie =
    name + "=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/;";

private def handleDiscussionActionConfirmed(
  event: DiscussionActionConfirmed,
): (Option[TopicId], Boolean) =
  event match {
    case DiscussionActionConfirmed.Delete(topic) =>
      (Some(topic), true)
    case DiscussionActionConfirmed.Vote(topic, _) =>
      (Some(topic), false)
    case DiscussionActionConfirmed.RemoveVote(topic, _) =>
      (Some(topic), false)
    case DiscussionActionConfirmed.Rename(topicId, _) =>
      (Some(topicId), false)
    case DiscussionActionConfirmed.UpdateRoomSlot(topicId, _) =>
      (Some(topicId), false)
    case DiscussionActionConfirmed.Unschedule(topicId) =>
      (Some(topicId), false)
    case DiscussionActionConfirmed.MoveTopic(topicId, _) =>
      (Some(topicId), false)
    case DiscussionActionConfirmed.SwapTopics(topic1, _, _, _) =>
      (Some(topic1), false)
    case DiscussionActionConfirmed.AddResult(_) =>
      (None, false)
    case DiscussionActionConfirmed.Rejected(_) =>
      (None, false)
  }

import io.laminext.websocket.*
val topicUpdates
  : WebSocket[DiscussionActionConfirmed, WebSocketMessage] = {
  // If I don't confine the scope of it, it clashes with laminar's `span`. Weird.
  import scala.concurrent.duration._
  WebSocket
    .url("/discussions")
    .text[DiscussionActionConfirmed, WebSocketMessage](
      _.toJson,
      _.fromJson[DiscussionActionConfirmed].left.map(Exception(_)),
    )
    .build(autoReconnect = true,
           reconnectDelay = 1.second,
           reconnectDelayOffline = 20.seconds,
           reconnectRetries = 10,
    )

}

def ticketCenter(
  topicUpdates: WebSocket[DiscussionActionConfirmed, WebSocketMessage],
) =
  div(
    FetchStream.get(
      "/ticket",
      fetchOptions =>
        fetchOptions.headers(
          "Authorization" -> s"Bearer ${getCookie("access_token").get}",
        ),
    ) --> { (responseText: String) =>
      val ticket = responseText
        .fromJson[Ticket]
        .getOrElse(
          throw new Exception(
            "Failed to parse ticket: " + responseText,
          ),
        )
      println("Ticket received: " + ticket)
      topicUpdates.sendOne(ticket)
    },
    topicUpdates.received.flatMapSwitch {
      (event: DiscussionActionConfirmed) =>
        event match
          case DiscussionActionConfirmed.Rejected(
                discussionAction,
              ) =>
            FetchStream
              .get(
                "/ticket",
                fetchOptions =>
                  fetchOptions.headers(
                    "Authorization" -> s"Bearer ${getCookie("access_token").get}",
                  ),
              )
              .map(response => (response, discussionAction))
          case other =>
            EventStream.empty
    } --> {
      (
        ticketResponse,
        discussionAction,
      ) =>
        val ticket = ticketResponse
          .fromJson[Ticket]
          .getOrElse(
            throw new Exception(
              "Failed to parse ticket: " + ticketResponse,
            ),
          )
        topicUpdates.sendOne(ticket)
        topicUpdates.sendOne(
          discussionAction,
        )
    },
  )
