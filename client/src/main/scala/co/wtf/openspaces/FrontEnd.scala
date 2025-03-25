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


private def SingleDiscussionComponent(
                                       name: StrictSignal[Person],
                                       topicUpdates: DiscussionAction => Unit,
                                       updateTargetDiscussion: Observer[Discussion],
                                     //
                                       signal: Signal[Option[Discussion]],
                                       transition: Option[Transition]
                                     ) = {
  signal.map {
    case Some(topic) =>
      val votePosition = topic.interestedParties.find(_.voter == name.now())
      val backgroundColorByPosition = "#C6DAD7"

      val $characters: List[(String, Int)] =
        topic.topic.unwrap.split("").zipWithIndex.toList

      val feedbackOnTopic =
        topic.interestedParties.find(_.voter == name.now())

      val ableToVoiceNegativity =
        feedbackOnTopic match
          case None | Some(Feedback(_, VotePosition.Interested)) => true
          case Some(Feedback(_, VotePosition.NotInterested)) => false

      val ableToVoicePositivity =
        feedbackOnTopic match
          case None | Some(Feedback(_, VotePosition.NotInterested)) => true
          case Some(Feedback(_, VotePosition.Interested)) => false

//        signal.map(_.topic.unwrap).map(_.split("").zipWithIndex.toList)
      div(cls := "TopicCard", // TODO Make this a component than can be used in the schedule view!
        backgroundColor := backgroundColorByPosition,
        transition match
          case Some(value) => value.height
          case None => height("15vh")
          ,
          div(
            cls := "MainActive",
//            justifyContent := "space-between",
                topic.topic.unwrap
//                $characters

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
            if (List("bill", "emma").exists(admin => name.now().unwrap.toLowerCase().contains(admin)))
              button(
                cls := "delete-topic",
                color := "red",
                border := "none", backgroundColor := "transparent", onClick --> Observer {
                  _ =>
                    // TODO Make sure this updates the ActiveDiscussion, so it's not left lingering on the schedule.
                    topicUpdates(DiscussionAction.Delete(topic.id))
                },
                "x"
              )
            else span()
          ),
            span(
              cls := "SecondaryActive",
              (votePosition match
                case Some(position) =>
                  span(
                    SvgIcon(topic.glyphicon),
                    p(topic.facilitator.unwrap),
                    p("Votes ", topic.votes),
                    topic.roomSlot match {
                      case Some(roomSlot) =>
                        button(
                          onClick.mapTo(topic.copy(roomSlot = None)) --> updateTargetDiscussion,
                          "Unslot"
                        )
                      case None =>
                        "Not scheduled."
                    },
                    SvgIcon(GlyphiconUtils.schedule).amend(
                      onClick.mapTo(topic) --> updateTargetDiscussion
                    )
                  )
                case None =>
                  span())
              ,
            ),
              div(
                cls := "ControlsActive",
                if (ableToVoiceNegativity)
                  span(
                    button(
                      cls := "AddButton", onClick --> Observer {
                        _ =>

                          topicUpdates(DiscussionAction.RemoveVote(topic.id, name.now()))
                          topicUpdates(DiscussionAction.Vote(topic.id, Feedback(name.now(), VotePosition.NotInterested)))
                      },
                      img(src := "./plus-icon-red.svg", role := "img")
                    ),
                  )
                else
                  span(
                    button(
                      cls := "AddButton disabled",
                      img(src := "./plus-icon-red.svg", role := "img"),
                    ),
                  )
                ,

                if (ableToVoicePositivity)
                  span(
                    button(
                      cls := "AddButton", onClick --> Observer {
                        _ =>
                          topicUpdates(DiscussionAction.RemoveVote(topic.id, name.now()))
                          topicUpdates(DiscussionAction.Vote(topic.id, Feedback(name.now(), VotePosition.Interested)))
                      },
                      img(src := "./plus-icon-green.svg", role := "img")
                    ),
                  )
                else
                  span(
                    button(
                      cls := "AddButton disabled",
                      img(src := "./plus-icon-green.svg", role := "img"),
                    ),
                  )
                ,
              )
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
  updateTargetDiscussion: Observer[Discussion]
) =
  div(
    cls := "TopicsContainer",
    children <--
      topicsOfInterest
        .splitTransition(_.id)(
          (index: TopicId, topic: Discussion, signal: Signal[Discussion], transition: Transition) =>
            div(
              child <-- SingleDiscussionComponent(name, topicUpdates, updateTargetDiscussion, signal.map(Some(_)), Some(transition))
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

  val reviewedTopics =
    localTopics
      .map(discussions => discussions.filter(d => d.interestedParties.exists(f => f.voter == name.now())))


  div(
    div("Unreviewed: "),
    DiscussionSubview(unreviewedTopics, None, name, topicUpdates, updateTargetDiscussion),
    hr(),
    div("Reviewed: "),
    DiscussionSubview(reviewedTopics, Some(VotePosition.Interested), name, topicUpdates, updateTargetDiscussion),
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
                           $activeDiscussion: StrictSignal[Option[Discussion]]
                         // TODO pass in the currnet discussion target, to decide whether to show a place empty or with the plus sign
                         ) =

  span(
    child <-- $discussionState.map { // TODO This should update the component whenever a Discussion is deleted.
      discussionState =>
        span(
          child <-- $activeDiscussion.map {
            discussionO =>
              discussionState.roomSlotContent(RoomSlot(room, timeSlot)) match
                case Some(value) =>
                  span(
                    onClick.mapTo(value) --> updateDiscussion, // TODO This is causing an unecesary update to be sent to server
                    SvgIcon(value.glyphicon).amend(cls := "filledTopic") // TODO amend always makes me suspicious
                  )
                case None =>
                  discussionO match
                    case Some(discussion) =>
                      discussion.roomSlot match
                        case Some(value) if (RoomSlot(room, timeSlot) == value) => // TODO Make this impossible
                          span(
                            cls := "glyphicon",
                            SvgIcon(discussion.glyphicon).amend(cls := "filledTopic") // TODO amend always makes me suspicious
                          )
                        case Some(_) =>
                          span(
                            cls := "glyphicon",
                            "-"
                          )
                        case None =>
                          span(
                            SvgIcon(GlyphiconUtils.plus),
                            onClick.mapTo(discussion.copy(roomSlot = Some(RoomSlot(room, timeSlot)))) --> updateDiscussion // TODO make updateDiscussion actually submit to server here
                          )
                    case None =>
                      span(
                        cls := "glyphicon",
                        "-"
                      )
          }
        )
    }
  )

def SlotSchedule(
                  timeOfSlot: String,
                  // This tuple is obviously terrible. TODO Figure out a better way
                  $discussionState: Signal[DiscussionState],
                  $timeSlotsForAllRooms: Signal[TimeSlotForAllRooms],
                  updateDiscussion: Observer[Discussion],
                  activeDiscussion: StrictSignal[Option[Discussion]]
                ) =
  div(
    cls:="SlotRow",
    div(cls:="TimeOfSlot", timeOfSlot),
    children <--
      $timeSlotsForAllRooms.map {
        timeSlotsForAllRooms =>
          timeSlotsForAllRooms.rooms
            .map {
              room =>
                div(cls:="Cell", ScheduleSlotComponent(timeSlotsForAllRooms.time, room, $discussionState, updateDiscussion, activeDiscussion))
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

def ScheduleView(
                  fullSchedule: Var[DiscussionState],
                  activeDiscussion: Var[Option[Discussion]],
                  topicUpdates: DiscussionAction => Unit,
                  name: StrictSignal[Person],
                  updateTargetDiscussion: Observer[Discussion]
                ) = {

  div(
    cls := "container",
    div(
      cls := "Targets",

      // TODO Use this once api is figured out:
      div(
        cls := "ActiveDiscussion Topic",
        child <-- SingleDiscussionComponent(name, topicUpdates, updateTargetDiscussion, activeDiscussion.signal, None),
      ),

//      div(
//        cls := "ActiveDiscussion Topic",
//        child <-- activeDiscussion.signal.map {
//          case Some(discussion) =>
//            div(
//              SvgIcon(discussion.glyphicon),
//              span(discussion.topic.unwrap),
//              discussion.roomSlot match {
//                case Some(roomSlot) =>
//                  button(
//                    onClick.mapTo(discussion.copy(roomSlot = None)) --> updateTargetDiscussion,
//                    "Unslot"
//                  )
//                case None =>
//                  "Not scheduled."
//              }
//            )
//          case None => span("nothing")
//        }
//      ),
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
        "8:00",
        fullSchedule.signal,
        fullSchedule.signal.map(discussionState => discussionState.slots(0)),
        updateTargetDiscussion,
        activeDiscussion.signal
      ),
      SlotSchedule(
        "9:20",
        fullSchedule.signal,
        fullSchedule.signal.map(discussionState => discussionState.slots(1)),
        updateTargetDiscussion,
        activeDiscussion.signal
      ),
      SlotSchedule(
        "10:30",
        fullSchedule.signal,
        fullSchedule.signal.map(discussionState => discussionState.slots(2)),
        updateTargetDiscussion,
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
    Var(DiscussionState(DiscussionState.timeSlotExamples, Map.empty)) //


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
      println("Should update target to: " + discussion)
      dom.document
        .getElementsByClassName("ActiveDiscussion").head
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
          topicUpdates.sendOne(DiscussionAction.UpdateRoomSlot(discussion.id, value))
        case None =>
          topicUpdates.sendOne(DiscussionAction.Unschedule(discussion.id))

      activeDiscussion.set(Some(discussion))
  }


  val app = {
    div(
      cls := "PageContainer",
      topicUpdates.connect,
      topicUpdates.received.tapEach(println(_)) --> Observer {
        (event: DiscussionActionConfirmed) =>
          println("Websocket Event: " + event)

          event match
            case DiscussionActionConfirmed.Delete(topic) =>
              if (activeDiscussion.now().map(_.id).contains(topic))
                activeDiscussion.set(None)
              else ()
            case _ => ()

          discussionState.update{existing =>
            val state = existing(event)
            val id:Option[TopicId] =
              event match
                case DiscussionActionConfirmed.Delete(topic) =>  Some(topic)
                case DiscussionActionConfirmed.Vote(topic, feedback) => Some(topic)
                case DiscussionActionConfirmed.RemoveVote(topic, voter) => Some(topic)
                case DiscussionActionConfirmed.Rename(topicId, newTopic) => Some(topicId)
                case DiscussionActionConfirmed.UpdateRoomSlot(topicId, roomSlot) => Some(topicId)
                case DiscussionActionConfirmed.Unschedule(topicId) => Some(topicId)
                case DiscussionActionConfirmed.AddResult(discussion) => None
            id.foreach(
              topicId =>
                if (activeDiscussion.now().map(_.id).contains(topicId))
                  activeDiscussion.set(state.data.get(topicId))
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
        setActiveDiscussion
      ),
      liveTopicSubmissionAndVoting(updateTargetDiscussion),
    )
  }

  render(container, app)