package co.wtf.openspaces

import animus.*
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import org.scalajs.dom.window
import zio.json.*

val localStorage = window.localStorage

private def getOrCreatePersistedName(): Var[Person] =
  val name =
    try {
      val retrieved =
        localStorage
          .getItem("name")
      Option.when(retrieved != null && !retrieved.isBlank)(
        Person(retrieved)
      )
    }
    catch {
      case e: Exception =>
        None
    }
  Var(name.getOrElse(Person("")))

private def BannerLogo() =
  div( width := "100%",
    img(cls := "LogoImg", src := "./wtf-web-nodate.jpg", role := "img")
  )

private def NameBadge(textVar: Var[Person]) =
  div(cls := "Banner",
    img(cls := "LogoImg", src := "./wtf-web-nodate.jpg", role := "img"),
    div(
      span("Name:"),
      input(
        placeholder := "Enter your name",
        value <-- textVar.signal.map(_.unwrap),
        onInput.mapToValue.map(Person(_)) --> textVar,
        textVar --> Observer {
          (value: Person) =>
            localStorage.setItem("name", value.unwrap)
        }
      ),
    )
  )

private def TopicSubmission(
                             submitEffect: Observer[DiscussionAction],
                             name: StrictSignal[Person],
                             setErrorMsg: Observer[Option[String]]
                           ) =
  val textVar = Var("")
  div( cls := "Flex",
    span(
      textArea(
        fontFamily := "Roboto", placeholder := "Create a topic...",
        value <-- textVar,
        onInput.mapToValue --> textVar,
      )
    ),
    button(
      onClick.mapTo(textVar.now())
        .map( s =>
          val res  = Topic.parse(s)
          res match
            case Left(value) =>
              setErrorMsg.onNext(Some(value))
              None
            case Right(value) =>
              Some(value)
        )
        .filter(_.isDefined)
        .map(_.get)
        .map(
          topicTitle => 
            DiscussionAction.Add(
              topicTitle, 
              name.now()
            )
        ) --> submitEffect,
      "Submit"
    )
  )

private def DiscussionSubview(
  topicsOfInterest: Signal[List[Discussion]],
  votePosition: Option[VotePosition],
  name: StrictSignal[Person],
  topicUpdates: DiscussionAction => Unit,
  updateTargetDiscussion: Observer[Discussion]
) =
  val backgroundColorByPosition =
    votePosition match
      case Some(position) => position match
        case VotePosition.Interested => "#6BB187"
        case VotePosition.NotInterested => "#FB6775"
      case None => "#C6DAD7"
  div(
    cls := "TopicsContainer",
    children <--
      topicsOfInterest

        .splitTransition(_.id)(
          (index, topic, signal, transition) =>
            val $characters: Signal[List[(String, Int)]] =
              signal.map(_.topic.unwrap).map(_.split("").zipWithIndex.toList)
            div(cls := "TopicCard",
              backgroundColor := backgroundColorByPosition,
              transition.height,
              div(cls := "TopicBody",
                div(
                  display.inlineFlex,
                  flexWrap := "wrap",
                  justifyContent := "space-between",
                  span(
                    div(
                      children <-- $characters.splitTransition(identity) {
                        case (_, (character, _), _, transition) =>
                          val newCharacter = character match
                            case " " => '\u00A0'
                            case _ => character.charAt(0)
                          div(
                            newCharacter,
                            display.inlineFlex,
                            transition.width,
                            //                              transition.height
                          )
                      }
                    ),
                  ),
                  if (List("bill", "emma").exists(admin => name.now().unwrap.toLowerCase().contains(admin)))
                    button(
                      cls := "delete-topic",
                      color := "red",
                      border := "none", backgroundColor := "transparent", onClick --> Observer {
                        _ =>
                          topicUpdates(DiscussionAction.Delete(topic.id))
                      },
                      "x"
                    )
                  else span()
                ),
                span(
                  cls := "VoteContainer",
                  child <-- signal.map(
                    topicLive =>
                      if topicLive.interestedParties.map(_.voter).contains(name.now()) then
                        span()
                      else
                        span(
                          button(
                            cls := "AddButton", onClick --> Observer {
                              _ =>
                                topicUpdates(DiscussionAction.Vote(topicLive.id, Feedback(name.now(), VotePosition.NotInterested)))
                            },
                            img(src := "./plus-icon-red.svg", role := "img")
                          ),
                        )
                  ),
                  (votePosition match
                    case Some(position) =>
                      span(
                        SvgIcon(topic.glyphicon),
                        p(topic.facilitator.unwrap),
                        p("Votes ", child <-- signal.map(_.votes), " "),
                        SvgIcon(GlyphiconUtils.schedule).amend(
                          onClick.mapTo(topic) --> updateTargetDiscussion
                        )
                      )
                    case None =>
                      span()),
                  child <-- signal.map(
                    topicLive =>
                      if topicLive.interestedParties.map(_.voter).contains(name.now()) then
                        button(
                          cls := "RemoveButton", onClick --> Observer {
                            _ =>
                              topicUpdates(DiscussionAction.RemoveVote(topicLive.id, name.now()))
                          },
                          "-"
                          //                    img(src := "./minus-icon.svg", role := "img") // TODO can we get a minus icon?
                        )
                      else
                        span(
                          button(
                            cls := "AddButton", onClick --> Observer {
                              _ =>
                                topicUpdates(DiscussionAction.Vote(topicLive.id, Feedback(name.now(), VotePosition.Interested)))
                            },
                            img(src := "./plus-icon-green.svg", role := "img")
                          ),
                        )
                  )
                )
              )
            )
        )
  )

private def DiscussionsToReview(
                                 topics: Signal[DiscussionState],
                                 name: StrictSignal[Person],
                                 topicUpdates: DiscussionAction => Unit,
                                 updateTargetDiscussion: Observer[Discussion]
                               ) =
  val localTopics = topics.map(_.data.values.toList)
  val topicsOfInterest: Signal[List[Discussion]] =
    localTopics
      .map(discussions => discussions.filter(d => d.interestedParties.contains(Feedback(name.now(), VotePosition.Interested))))

  val topicsWithoutInterest =
    localTopics
      .map(discussions => discussions.filter(d => d.interestedParties.contains(Feedback(name.now(), VotePosition.NotInterested))))

  val unreviewedTopics =
    localTopics
      .map(discussions => discussions.filterNot(d => d.interestedParties.exists(f => f.voter == name.now())))


  div(
    div("Unreviewed: "),
    DiscussionSubview(unreviewedTopics, None, name, topicUpdates, updateTargetDiscussion),
    hr(),
    div("Interested: "),
    DiscussionSubview(topicsOfInterest, Some(VotePosition.Interested), name, topicUpdates, updateTargetDiscussion),
    hr(),
    div("Not Interested: "),
    DiscussionSubview(topicsWithoutInterest, Some(VotePosition.NotInterested), name, topicUpdates, updateTargetDiscussion),
  )



enum AppView:
  case Home
  case ScheduleView
  case SubmitTopic

def ScheduleSlotComponent(
                           timeSlot: TimeSlot,
                           room: Room,
                           discussionState: DiscussionState,
                           updateDiscussion: Observer[Discussion],
                           $activeDiscussion: StrictSignal[Option[Discussion]]
                         // TODO pass in the currnet discussion target, to decide whether to show a place empty or with the plus sign
                         ) =

    discussionState.roomSlotContent(RoomSlot(room, timeSlot)) match
      case Some(value) =>
        span(
          onClick.mapTo(value) --> updateDiscussion,
          SvgIcon(value.glyphicon).amend(cls := "filledTopic") // TODO amend always makes me suspicious
        )
      case None =>
        span(
          child <-- $activeDiscussion.map {
            case Some(discussion) =>
              discussion.roomSlot match
                case Some(value) =>// TODO Make this impossible
                  if (value == RoomSlot(room, timeSlot))
                    span(
                      cls := "glyphicon",
                      SvgIcon(discussion.glyphicon).amend(cls := "filledTopic") // TODO amend always makes me suspicious
                    )
                  else
                    span(
                      cls := "glyphicon",
                      "-"
                    )
                case None =>
                  println("Should show a plus")
                  span(
                    SvgIcon(GlyphiconUtils.plus),
                    onClick.mapTo(discussion.copy(roomSlot = Some(RoomSlot(room, timeSlot)))) --> updateDiscussion
                  )
            case None =>
              println("showing a boring dash")
              span(
                cls := "glyphicon",
                "-"
              )
          }
        )

def SlotSchedule(
                  timeOfSlot: String,
                  // This tuple is obviously terrible. TODO Figure out a better way
                  $timeSlotsForAllRooms: Signal[(TimeSlotForAllRooms, DiscussionState)]
                  , updateDiscussion: Observer[Discussion],
                  activeDiscussion: StrictSignal[Option[Discussion]]
                ) =
  div(
    cls:="SlotRow",
    div(cls:="TimeOfSlot", timeOfSlot),
    children <--
      $timeSlotsForAllRooms.map {
        (timeSlotsForAllRooms, discussionState) =>
          timeSlotsForAllRooms.rooms
            .map {
              room =>
                div(cls:="Cell", ScheduleSlotComponent(timeSlotsForAllRooms.time, room, discussionState, updateDiscussion, activeDiscussion))
            }
      }
  )

case class ErrorBanner(
                        error: Var[Option[String]] =
                        Var(None)
                      ):
  val component =
    div(
      child <--
        error.signal.map {
          case Some(value) =>
            div(
              cls := "Error",
              color := "red",
              "Error: " + value
            )
          case None =>
            div()
        }
    )

def ScheduleView(fullSchedule: Var[DiscussionState], activeDiscussion: Var[Option[Discussion]]) = {

  val setActiveDiscussion: Observer[Discussion] = Observer {
    discussion => activeDiscussion.set(Some(discussion))
  }

  div(
    cls := "container",
    div(
      cls := "Targets",
      div(
        cls := "ActiveDiscussion Topic",
        child <-- activeDiscussion.signal.map {
          case Some(discussion) =>
            div(
              SvgIcon(discussion.glyphicon),
              span(discussion.topic.unwrap)
            )
          case None => span("nothing")
        }
      ),
    ),
    div(
      cls := "Schedule",
      div(
        cls := "RoomHeaders",
        div(cls := "Room1", "King"),
        div(cls := "Room2", "Hawk"),
        div(cls := "Room3", "Art!"),
        div(cls := "Room4", "Dance")
      ),
      SlotSchedule(
        "1",
        fullSchedule.signal.map(discussionState => (discussionState.slots(0), discussionState)),
        setActiveDiscussion,
        activeDiscussion.signal
      ),
      SlotSchedule(
        "2",
        fullSchedule.signal.map(discussionState => (discussionState.slots(1), discussionState)),
        setActiveDiscussion,
        activeDiscussion.signal
      ),
    ),
  )
}

object FrontEnd extends App:
  lazy val container = dom.document.getElementById("app")
  import io.laminext.websocket.*

  val topicUpdates =
    WebSocket.url("/discussions").text[DiscussionActionConfirmed, DiscussionAction](
      _.toJson,
      _.fromJson[DiscussionActionConfirmed].left.map(Exception(_))
    ).build()

  val discussionState: Var[DiscussionState] =
    Var(DiscussionState.example)


  val errorBanner =
    ErrorBanner()

  val submitNewTopic: Observer[DiscussionAction] = Observer {
    case discussion@(add: DiscussionAction.Add) =>
      if (add.facilitator.unwrap.trim.length < 2)
        errorBanner.error.set(Some("User name too short. Tell us who you are!"))
      else
        errorBanner.error.set(None)
        topicUpdates.sendOne(discussion)
    case _ => ()
  }

  val name = getOrCreatePersistedName()
  def liveTopicSubmissionAndVoting(updateTargetDiscussion: Observer[Discussion]) =
    div(
      TopicSubmission(submitNewTopic, name.signal, errorBanner.error.toObserver),
      DiscussionsToReview(discussionState.signal, name.signal, topicUpdates.sendOne, updateTargetDiscussion),
    )

  val activeDiscussion: Var[Option[Discussion]] =
    Var(None)

  val updateTargetDiscussion: Observer[Discussion] = Observer[Discussion] {
    discussion =>
      activeDiscussion.set(Some(discussion))
  }

  val app = {
    div(
      cls := "PageContainer",
      topicUpdates.connect,
      topicUpdates.received.tapEach(println(_)) --> Observer {
        (event: DiscussionActionConfirmed) =>
          println("Websocket Event: " + event)
          discussionState.update(existing =>
            existing(event)
          )
      },
      errorBanner.component,
      NameBadge(name),
      ScheduleView(discussionState, activeDiscussion),
      liveTopicSubmissionAndVoting(updateTargetDiscussion),
    )
  }

  render(container, app)