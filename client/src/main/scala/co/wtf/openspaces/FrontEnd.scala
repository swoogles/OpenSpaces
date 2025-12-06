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

/** Captures the bounding rectangle of an element for menu positioning
  */
case class MenuRect(
  left: Double,
  top: Double,
  right: Double,
  bottom: Double,
  width: Double,
  height: Double)

object MenuRect:
  def fromElement(
    element: org.scalajs.dom.Element,
  ): MenuRect =
    val rect = element.getBoundingClientRect()
    MenuRect(rect.left,
             rect.top,
             rect.right,
             rect.bottom,
             rect.width,
             rect.height,
    )

/** Centralized menu positioning calculations */
object MenuPositioning:
  private val menuMargin = 10.0
  private val smallScreenThreshold = 400.0
  private val standardMenuMaxWidth = 350.0

  /** Standard positioning for dropdown menus (swap, unscheduled,
    * active discussion)
    */
  def standardMenuPosition(
    rect: MenuRect,
  ): (Double, Double) =
    val viewportHeight = dom.window.innerHeight
    val viewportWidth = dom.window.innerWidth

    val menuWidth =
      Math.min(standardMenuMaxWidth, viewportWidth - 2 * menuMargin)

    val x =
      if (viewportWidth <= smallScreenThreshold) menuMargin
      else rect.left + rect.width / 2 - menuWidth / 2

    val clampedX =
      if (viewportWidth <= smallScreenThreshold) menuMargin
      else
        Math.max(menuMargin,
                 Math.min(x, viewportWidth - menuWidth - menuMargin),
        )

    // Position near top of screen to ensure visibility
    val clampedY = Math.max(20.0, viewportHeight * 0.05)

    (clampedX, clampedY)

  /** Positioning for popovers that appear near the triggering element
    */
  def popoverPosition(
    rect: MenuRect,
    popoverWidth: Double,
    popoverHeight: Double,
  ): (Double, Double) =
    val viewportHeight = dom.window.innerHeight
    val viewportWidth = dom.window.innerWidth

    val x = rect.left + rect.width / 2 - popoverWidth / 2

    // If popover would go off bottom of screen, position above instead
    val y =
      if (rect.bottom + popoverHeight + 5 > viewportHeight)
        rect.top - popoverHeight - 5
      else rect.bottom + 5

    // Clamp x position to stay within viewport
    val clampedX =
      Math.max(5, Math.min(x, viewportWidth - popoverWidth - 5))

    (clampedX, y)

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

  // Popover state: (Discussion, triggering element rect)
  val popoverState: Var[Option[(Discussion, MenuRect)]] =
    Var(None)

  // Swap action menu state: (selected Discussion, target Discussion, triggering element rect)
  val swapMenuState: Var[Option[(Discussion, Discussion, MenuRect)]] =
    Var(None)

  // Unscheduled discussions menu state: (RoomSlot, triggering element rect)
  val unscheduledMenuState: Var[Option[(RoomSlot, MenuRect)]] =
    Var(None)

  // Active discussion actions menu state: (Discussion, triggering element rect)
  val activeDiscussionMenuState: Var[Option[(Discussion, MenuRect)]] =
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
        case Some((selectedDiscussion, targetDiscussion, rect)) =>
          SwapActionMenu(
            selectedDiscussion,
            targetDiscussion,
            rect,
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
          case (Some((roomSlot, rect)), discussionState) =>
            val unscheduledDiscussions = discussionState.data.values
              .filter(_.roomSlot.isEmpty)
              .toList
            UnscheduledDiscussionsMenu(
              unscheduledDiscussions,
              roomSlot,
              rect,
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
        case Some((discussion, rect)) =>
          ActiveDiscussionActionMenu(
            discussion,
            rect,
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
  showPopover: Observer[(Discussion, MenuRect)],
  showSwapMenu: Observer[(Discussion, Discussion, MenuRect)],
  showUnscheduledMenu: Observer[(RoomSlot, MenuRect)],
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
              span(
                onClick --> Observer {
                  (event: org.scalajs.dom.MouseEvent) =>
                    event.stopPropagation()
                    val rect = MenuRect.fromElement(
                      event.currentTarget
                        .asInstanceOf[org.scalajs.dom.Element],
                    )
                    showPopover.onNext((value, rect))
                },
                // Long-press to show swap menu when there's an active discussion
                if (isSwappable)
                  onContextMenu.preventDefault --> Observer {
                    (event: org.scalajs.dom.MouseEvent) =>
                      event.stopPropagation()
                      discussionO.foreach { activeDiscussion =>
                        val rect = MenuRect.fromElement(
                          event.currentTarget
                            .asInstanceOf[org.scalajs.dom.Element],
                        )
                        showSwapMenu.onNext(
                          (activeDiscussion, value, rect),
                        )
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
                        onClick --> Observer {
                          (event: org.scalajs.dom.MouseEvent) =>
                            event.stopPropagation()
                            val rect = MenuRect.fromElement(
                              event.currentTarget
                                .asInstanceOf[org.scalajs.dom.Element],
                            )
                            showUnscheduledMenu.onNext(
                              (RoomSlot(room, timeSlot), rect),
                            )
                        },
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
                    onClick --> Observer {
                      (event: org.scalajs.dom.MouseEvent) =>
                        event.stopPropagation()
                        val rect = MenuRect.fromElement(
                          event.currentTarget
                            .asInstanceOf[org.scalajs.dom.Element],
                        )
                        showUnscheduledMenu.onNext(
                          (RoomSlot(room, timeSlot), rect),
                        )
                    },
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
  showPopover: Observer[(Discussion, MenuRect)],
  showSwapMenu: Observer[(Discussion, Discussion, MenuRect)],
  showUnscheduledMenu: Observer[(RoomSlot, MenuRect)],
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

def SwapActionMenu(
  selectedDiscussion: Discussion,
  targetDiscussion: Discussion,
  triggerRect: MenuRect,
  topicUpdates: DiscussionAction => Unit,
  dismissMenu: Observer[Unit],
) =
  val (x, y) = MenuPositioning.standardMenuPosition(triggerRect)
  div(
    cls := "SwapActionMenu",
    left := s"${x}px",
    top := s"${y}px",
    onClick.preventDefault.stopPropagation --> Observer(_ => ()),
    div(cls := "SwapActionMenu-header", "Actions"),
    // Selected topic (current selection)
    div(
      cls := "SwapActionMenu-section",
      div(cls := "SwapActionMenu-label", "Selected Topic:"),
      div(
        cls := "SwapActionMenu-topic SwapActionMenu-topic--selected",
        SvgIcon(selectedDiscussion.glyphicon),
        div(
          div(cls := "SwapActionMenu-topicName",
              selectedDiscussion.topicName,
          ),
          div(
            cls := "SwapActionMenu-roomSlot",
            selectedDiscussion.roomSlot
              .map(_.displayString)
              .getOrElse("Unscheduled"),
          ),
        ),
      ),
    ),
    // Target topic
    div(
      cls := "SwapActionMenu-section SwapActionMenu-section--target",
      div(cls := "SwapActionMenu-label", "Target Topic:"),
      div(
        cls := "SwapActionMenu-topic SwapActionMenu-topic--target",
        SvgIcon(targetDiscussion.glyphicon),
        div(
          div(cls := "SwapActionMenu-topicName",
              targetDiscussion.topicName,
          ),
          div(
            cls := "SwapActionMenu-roomSlot",
            targetDiscussion.roomSlot
              .map(_.displayString)
              .getOrElse("Unscheduled"),
          ),
        ),
      ),
    ),
    // Action buttons
    div(
      cls := "SwapActionMenu-actions",
      button(
        cls := "SwapActionMenu-swapButton",
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
        cls := "SwapActionMenu-cancelButton",
        onClick.mapToUnit --> dismissMenu,
        "Cancel",
      ),
    ),
  )

def UnscheduledDiscussionsMenu(
  unscheduledDiscussions: List[Discussion],
  targetRoomSlot: RoomSlot,
  triggerRect: MenuRect,
  facilitator: StrictSignal[Person],
  topicUpdates: DiscussionAction => Unit,
  dismissMenu: Observer[Unit],
  setActiveDiscussion: Observer[Discussion],
) =
  val (x, y) = MenuPositioning.standardMenuPosition(triggerRect)
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
    cls := "SwapActionMenu",
    left := s"${x}px",
    top := s"${y}px",
    onClick.preventDefault.stopPropagation --> Observer(_ => ()),
    div(cls := "SwapActionMenu-header", "Assign Discussion"),
    div(
      cls := "SwapActionMenu-section",
      div(cls := "SwapActionMenu-label",
          s"Room Slot: ${targetRoomSlot.displayString}",
      ),
    ),
    div(
      cls := "SwapActionMenu-section",
      div(cls := "SwapActionMenu-label", "Add New Discussion:"),
      textArea(
        cls := "SwapActionMenu-textArea",
        placeholder := "Describe the discussion to schedule...",
        value <-- textVar.signal,
        onInput.mapToValue --> textVar,
      ),
      button(
        cls := "SwapActionMenu-swapButton",
        onClick --> Observer { _ =>
          submitNewDiscussion()
        },
        "Add & Assign",
      ),
      child <--
        errorVar.signal.map {
          case Some(errorMessage) =>
            div(cls := "SwapActionMenu-error", errorMessage)
          case None =>
            span()
        },
    ),
    div(
      cls := "SwapActionMenu-section",
      div(cls := "SwapActionMenu-label", "Available Discussions:"),
      if (unscheduledDiscussions.isEmpty) {
        div(
          cls := "SwapActionMenu-topic",
          div(
            div(cls := "SwapActionMenu-topicName",
                "No unscheduled discussions available",
            ),
          ),
        )
      }
      else {
        div(
          cls := "SwapActionMenu-actions",
          unscheduledDiscussions.map { discussion =>
            button(
              cls := "SwapActionMenu-swapButton",
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
                cls := "SwapActionMenu-topicName",
                discussion.topicName,
              ),
            )
          },
        )
      },
    ),
    div(
      cls := "SwapActionMenu-actions",
      button(
        cls := "SwapActionMenu-cancelButton",
        onClick.mapToUnit --> dismissMenu,
        "Cancel",
      ),
    ),
  )

def ActiveDiscussionActionMenu(
  discussion: Discussion,
  triggerRect: MenuRect,
  topicUpdates: DiscussionAction => Unit,
  dismissMenu: Observer[Unit],
) =
  val (x, y) = MenuPositioning.standardMenuPosition(triggerRect)
  val isScheduled = discussion.roomSlot.isDefined

  val cancelButton =
    button(
      cls := "SwapActionMenu-cancelButton",
      onClick.mapToUnit --> dismissMenu,
      "Close",
    )

  val actionElements: List[Modifier[HtmlElement]] =
    if isScheduled then
      List(
        button(
          cls := "SwapActionMenu-swapButton",
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
          cls := "SwapActionMenu-topic SwapActionMenu-topic--selected",
          span(
            cls := "SwapActionMenu-topicName",
            "This discussion is not scheduled yet.",
          ),
        ),
        cancelButton,
      )

  val actionSection =
    (cls := "SwapActionMenu-actions") :: actionElements

  div(
    cls := "SwapActionMenu",
    left := s"${x}px",
    top := s"${y}px",
    onClick.preventDefault.stopPropagation --> Observer(_ => ()),
    div(cls := "SwapActionMenu-header", "Discussion actions"),
    div(
      cls := "SwapActionMenu-topic SwapActionMenu-topic--selected",
      SvgIcon(discussion.glyphicon),
      div(
        div(
          cls := "SwapActionMenu-topicName",
          discussion.topicName,
        ),
        div(
          cls := "SwapActionMenu-roomSlot",
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
  showActiveDiscussionMenu: Observer[(Discussion, MenuRect)],
): Binder[HtmlElement] =
  onContextMenu.preventDefault --> Observer {
    (event: org.scalajs.dom.MouseEvent) =>
      event.stopPropagation()
      activeDiscussionNow().foreach { discussion =>
        if (discussion.roomSlot.isDefined) {
          val rect = MenuRect.fromElement(
            event.currentTarget.asInstanceOf[org.scalajs.dom.Element],
          )
          showActiveDiscussionMenu.onNext((discussion, rect))
        }
      }
  }

def ScheduleView(
  fullSchedule: Var[DiscussionState],
  activeDiscussion: Var[Option[Discussion]],
  topicUpdates: DiscussionAction => Unit,
  name: StrictSignal[Person],
  updateTargetDiscussion: Observer[Discussion],
  popoverState: Var[Option[(Discussion, MenuRect)]],
  swapMenuState: Var[Option[(Discussion, Discussion, MenuRect)]],
  unscheduledMenuState: Var[Option[(RoomSlot, MenuRect)]],
  activeDiscussionMenuState: Var[Option[(Discussion, MenuRect)]],
) =
  val showPopover: Observer[(Discussion, MenuRect)] =
    Observer { case (discussion, rect) =>
      popoverState.set(Some((discussion, rect)))
    }

  val showSwapMenu: Observer[(Discussion, Discussion, MenuRect)] =
    Observer { case (selected, target, rect) =>
      swapMenuState.set(Some((selected, target, rect)))
    }

  val showUnscheduledMenu: Observer[(RoomSlot, MenuRect)] =
    Observer { case (roomSlot, rect) =>
      unscheduledMenuState.set(Some((roomSlot, rect)))
    }

  val showActiveDiscussionMenu: Observer[(Discussion, MenuRect)] =
    Observer { case (discussion, rect) =>
      activeDiscussionMenuState.set(Some((discussion, rect)))
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
  showPopover: Observer[(Discussion, MenuRect)],
  showSwapMenu: Observer[(Discussion, Discussion, MenuRect)],
  showUnscheduledMenu: Observer[(RoomSlot, MenuRect)],
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
