package co.wtf.openspaces

import animus.*
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import org.scalajs.dom.window
import zio.json.*

val localStorage = window.localStorage

import org.scalajs.dom
import scala.scalajs.js.URIUtils

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
          val res = Topic.parse(s)
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
          println(
            "Unsafely clearing out submission, regardless of whether it got to the server",
          )
          println(
            "Should wait till the Server confirms a TopicAdd, and if it matches the current text, clear it out",
          )
          println("Also a nice submission animation should happen")
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
) = {
  signal.map {
    case Some(topic) =>
      val votePosition =
        topic.interestedParties.find(_.voter == name.now())
      val backgroundColorByPosition = "#C6DAD7"

      val $characters: List[(String, Int)] =
        topic.topic.unwrap.split("").zipWithIndex.toList

      val feedbackOnTopic =
        topic.interestedParties.find(_.voter == name.now())

      val hasExpressedInterest =
        feedbackOnTopic match
          case Some(Feedback(_, VotePosition.Interested)) => true
          case _                                          => false

      div(
        cls := "TopicCard", // TODO Make this a component than can be used in the schedule view!
        backgroundColor := backgroundColorByPosition,
        transition match
          case Some(value) => value.height
          case None        => height("15vh")
        ,
        div(
          cls := "MainActive",
          topic.topic.unwrap

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
          votePosition match
            case Some(position) =>
              span(
                SvgIcon(topic.glyphicon),
                span(topic.facilitator.unwrap),
                span("Votes ", topic.votes),
                topic.roomSlot match {
                  case Some(roomSlot) =>
                    div(
                      roomSlot.timeSlot.s + " " + roomSlot.room.name,
                    )
                  case None =>
                    div("Unscheduled")
                },
              )
            case None =>
              span(),
        ),
        div(
          cls := "ControlsActive",
          if (hasExpressedInterest)
            span(
              button(
                cls := "AddButton",
                onClick --> Observer { _ =>
                  topicUpdates(
                    DiscussionAction.RemoveVote(topic.id, name.now()),
                  )
                  topicUpdates(
                    DiscussionAction.Vote(
                      topic.id,
                      Feedback(name.now(), VotePosition.NotInterested),
                    ),
                  )
                },
                SvgIcon(GlyphiconUtils.heart),
              ),
            )
          else
            span(
              button(
                cls := "AddButton",
                onClick --> Observer { _ =>

                  topicUpdates(
                    DiscussionAction.RemoveVote(topic.id, name.now()),
                  )
                  topicUpdates(
                    DiscussionAction.Vote(
                      topic.id,
                      Feedback(name.now(), VotePosition.Interested),
                    ),
                  )
                },
                SvgIcon(GlyphiconUtils.heartEmpty),
              ),
            ),
          topic.roomSlot match {
            case Some(roomSlot) =>
              button(
                onClick.mapTo(
                  topic.copy(roomSlot = None),
                ) --> updateTargetDiscussion,
                "Reschedule",
              )
            case None =>
              "Unscheduled"
          },
          SvgIcon(GlyphiconUtils.schedule).amend(
            onClick.mapTo(topic) --> updateTargetDiscussion,
          ),
        ),
      )
    case None =>
      println("failed to get a topic")
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
              span(
                onClick.mapTo(
                  value,
                ) --> updateDiscussion, // TODO This is causing an unecesary update to be sent to server
                SvgIcon(value.glyphicon).amend(
                  cls := s"filledTopic $selectedTopicStyling",
                ), // TODO amend always makes me suspicious
              )
            case None =>
              discussionO match
                case Some(discussion) =>
                  discussion.roomSlot match
                    case Some(value)
                        if RoomSlot(room,
                                    timeSlot,
                        ) == value => // TODO Make this impossible
                      span(
                        cls := "glyphicon",
                        SvgIcon(discussion.glyphicon).amend(
                          cls := "filledTopic",
                        ), // TODO amend always makes me suspicious
                      )
                    case Some(_) =>
                      span(
                        cls := "glyphicon",
                        "-",
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
                  span(
                    cls := "glyphicon",
                    "-",
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
) =
  div(
    child <--
      $timeSlotsForAllRooms.map { timeSlotsForAllRooms =>
        div(
          cls := "SlotRow",
          div(cls := "TimeOfSlot", timeSlotsForAllRooms.time.s),
          timeSlotsForAllRooms.rooms
            .map { room =>
              div(cls := "Cell",
                  ScheduleSlotComponent(timeSlotsForAllRooms.time,
                                        room,
                                        $discussionState,
                                        updateDiscussion,
                                        activeDiscussion,
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
              cls := "Error",
              color := "red",
              "Error: " + value,
            )
          case None =>
            div()
        },
    )

def ScheduleView(
  fullSchedule: Var[DiscussionState],
  activeDiscussion: Var[Option[Discussion]],
  topicUpdates: DiscussionAction => Unit,
  name: StrictSignal[Person],
  updateTargetDiscussion: Observer[Discussion],
) =
  div(
    cls := "container",
    div(
      cls := "Targets",
      div(
        cls := "ActiveDiscussion Topic",
        child <-- SingleDiscussionComponent(name,
                                            topicUpdates,
                                            updateTargetDiscussion,
                                            activeDiscussion.signal,
                                            None,
        ),
      ),
    ),
    div(
      cls := "Schedule",
      div(
        cls := "RoomHeaders",
        div(cls := "Room1", "King"),
        div(cls := "Room2", "Hawk"),
        div(cls := "Room3", "Art!"),
        div(cls := "Room4", "Dance"),
      ),
      div(
        cls := "TimeSlots",
        SlotSchedules(
          fullSchedule.signal,
          updateTargetDiscussion,
          activeDiscussion.signal,
        ),
      ),
    ),
  )

def SlotSchedules(
  $discussionState: Signal[DiscussionState],
  updateDiscussion: Observer[Discussion],
  activeDiscussion: StrictSignal[Option[Discussion]],
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
                      ),
                    )
                  },
              ),
            ),
          ),
        ),
      ),
  )

object FrontEnd extends App:
  lazy val container = dom.document.getElementById("app")
  import io.laminext.websocket.*

  val topicUpdates = {
    // If I don't confine the scope of it, it clashes with laminar's `span`. Weird.
    import scala.concurrent.duration._
    WebSocket
      .url("/discussions")
      .text[DiscussionActionConfirmed, WebSocketMessage](
        _.toJson,
        _.fromJson[DiscussionActionConfirmed].left.map(Exception(_)),
      )
      .build(autoReconnect = true, reconnectDelay = 1.second, reconnectDelayOffline = 20.seconds, reconnectRetries = 10)
  }

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

  val updateTargetDiscussion: Observer[Discussion] =
    Observer[Discussion] { discussion =>
      println("Should update target to: " + discussion)
      dom.document
        .getElementsByClassName("ActiveDiscussion")
        .head
        .scrollIntoView(
          top = false,
          //              { behavior: "instant", block: "end" }
        )
      activeDiscussion.set(Some(discussion))
    }

  val setActiveDiscussion: Observer[Discussion] = Observer {
    discussion =>
      discussion.roomSlot match
        case Some(value) =>
          println("Doing things.")
          topicUpdates.sendOne(
            DiscussionAction.UpdateRoomSlot(discussion.id, value),
          )
        case None =>
          topicUpdates.sendOne(
            DiscussionAction.Unschedule(discussion.id),
          )

      activeDiscussion.set(Some(discussion))
  }

  val app =
    div(
      cls := "PageContainer",
      topicUpdates.connect,
      getCookie("access_token") match {
        case Some(accessToken) =>
          div(
            div(
              button(
                onClick --> Observer { _ =>
                  deleteCookie("access_token")
                  window.location.reload()
                },
                "Logout",
              ),
            ),
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
                    println("Oh no! We need to do a new ticket!")
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
                ) // REtry after successful ticket post
            },
            topicUpdates.received.tapEach(println(_)) --> Observer {
              (event: DiscussionActionConfirmed) =>

                discussionState.update { existing =>
                  val state = existing(event)
                  val id: Option[TopicId] =
                    event match
                      case DiscussionActionConfirmed.Delete(topic) =>
                        if (
                          activeDiscussion
                            .now()
                            .map(_.id)
                            .contains(topic)
                        )
                          activeDiscussion.set(None)
                        else ()
                        Some(topic)
                      case DiscussionActionConfirmed.Vote(topic,
                                                          feedback,
                          ) =>
                        Some(topic)
                      case DiscussionActionConfirmed.RemoveVote(topic,
                                                                voter,
                          ) =>
                        Some(topic)
                      case DiscussionActionConfirmed.Rename(topicId,
                                                            newTopic,
                          ) =>
                        Some(topicId)
                      case DiscussionActionConfirmed
                            .UpdateRoomSlot(topicId, roomSlot) =>
                        Some(topicId)
                      case DiscussionActionConfirmed.Unschedule(
                            topicId,
                          ) =>
                        Some(topicId)
                      case DiscussionActionConfirmed.AddResult(
                            discussion,
                          ) =>
                        None
                      case DiscussionActionConfirmed.Rejected(
                            action,
                          ) =>
                        // TODO Recognize when an action was rejected because the user was unticketed, so that we can:
                        //    - Request a ticket
                        //    - Submit the ticket
                        //    - Retry the action
                        None
                  id.foreach(topicId =>
                    if (
                      activeDiscussion
                        .now()
                        .map(_.id)
                        .contains(topicId)
                    )
                      activeDiscussion.set(state.data.get(topicId)),
                  )

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

def deleteCookie(
  name: String,
) =
  dom.document.cookie =
    name + "=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/;";
