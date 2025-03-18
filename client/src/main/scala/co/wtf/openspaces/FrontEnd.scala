package co.wtf.openspaces

import co.wtf.openspaces.{Discussion, Room, ScheduleSlot}
import com.raquo.laminar.api.L.{*, given}
import io.laminext.websocket.WebSocket
import org.scalajs.dom
import zio.json.*
import org.scalajs.dom.window
import animus.*

import scala.util.Random

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
  topicUpdates: DiscussionAction => Unit
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
                                 topicUpdates: DiscussionAction => Unit
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
    DiscussionSubview(unreviewedTopics, None, name, topicUpdates),
    hr(),
    div("Interested: "),
    DiscussionSubview(topicsOfInterest, Some(VotePosition.Interested), name, topicUpdates),
    hr(),
    div("Not Interested: "),
    DiscussionSubview(topicsWithoutInterest, Some(VotePosition.NotInterested), name, topicUpdates),
  )



enum AppView:
  case Home
  case ScheduleView
  case SubmitTopic

def ScheduleSlotComponent(timeSlot: TimeSlot, scheduleSlot: ScheduleSlot, updateDiscussion: Observer[ScheduledDiscussion]) =

    scheduleSlot.discussion match
      case Some(value) =>
        span(
          onClick.mapTo(ScheduledDiscussion(value, scheduleSlot.room, timeSlot)) --> updateDiscussion,
          GlyphiconRandomDemo.randomGlyphicon()
        )
      case None =>
        span(
          SvgIcon(GlyphiconUtils.plus)
        )

def SlotSchedule(timeOfSlot: String, $timeSlotsForAllRooms: Signal[TimeSlotForAllRooms], updateDiscussion: Observer[ScheduledDiscussion]) =
  div(
    cls:="SlotRow",
    div(cls:="TimeOfSlot", timeOfSlot),
    children <--
      $timeSlotsForAllRooms.map {
        timeSlotsForAllRooms =>
          timeSlotsForAllRooms.cells
            .map {
              cell =>
                div(cls:="Cell", ScheduleSlotComponent(timeSlotsForAllRooms.time, cell, updateDiscussion))
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

def ScheduleView() = {
  val activeDiscussion: Var[Option[ScheduledDiscussion]] =
    Var(None)

  val targetDiscussion: Var[Option[ScheduledDiscussion]] =
    Var(None)

  val setActiveDiscussion: Observer[ScheduledDiscussion] = Observer {
    discussion => activeDiscussion.set(Some(discussion))
  }

  val setTargetDiscussion: Observer[ScheduledDiscussion] = Observer {
    discussion => targetDiscussion.set(Some(discussion))
  }

  val settingActiveDiscussion: Var[Boolean] = Var(true) // Don't love the boolean here

  val updateDiscussion: Observer[ScheduledDiscussion] = Observer {
    discussion =>
      if (settingActiveDiscussion.now()) {
        activeDiscussion.set(Some(discussion))
      } else {
        targetDiscussion.set(Some(discussion))
      }
  }

  val swapDiscussions: Observer[Unit] = Observer {
    _ =>
      val tmp = activeDiscussion.now()
      activeDiscussion.set(targetDiscussion.now())
      targetDiscussion.set(tmp)
  }

  val fullSchedule =
    Var(
      FullSchedule(
        List(
          TimeSlotForAllRooms(
            TimeSlot("8:00-8:50"),
            List(ScheduleSlot(Room.king, Some(Discussion.example1)), ScheduleSlot(Room.hawk), ScheduleSlot(Room.artGallery, Some(Discussion.example2)), ScheduleSlot(Room.danceHall))
          ),
          TimeSlotForAllRooms(
            TimeSlot("9:20-10:10"),
            List(ScheduleSlot(Room.king, Some(Discussion.example3)), ScheduleSlot(Room.hawk), ScheduleSlot(Room.artGallery), ScheduleSlot(Room.danceHall))
          )
        )

      )
    )

  div(
    cls := "container",
    div(
      cls := "Targets",
      div(
        cls := "Swap",
        onClick.mapTo(()) --> swapDiscussions,
        SvgIcon(Glyphicon("glyphicons-basic-82-refresh.svg")),
      ),
      div(
        cls := "ActiveDiscussion Topic",
        onClick.mapTo(true) --> settingActiveDiscussion,
        child <-- activeDiscussion.signal.map {
          case Some(discussion) => span(discussion.discussion.topic.unwrap)
          case None => span("nothing")
        }
      ),
      div(
        cls := "SwapTarget Topic",
        onClick.mapTo(false) --> settingActiveDiscussion,
        child <-- targetDiscussion.signal.map {
          case Some(discussion) => span(discussion.discussion.topic.unwrap)
          case None => span("nothing")
        }
      )
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
        fullSchedule.signal.map(_.slots(0)),
        updateDiscussion
      ),
      SlotSchedule(
        "2",
        fullSchedule.signal.map(_.slots(1)),
        updateDiscussion
      ),
    ),
  )
}

object FrontEnd extends App:
  lazy val container = dom.document.getElementById("app")
  import io.laminext.websocket.*

  val topicUpdates =
    WebSocket.url("/discussions").text[DiscussionAction, DiscussionAction](
      _.toJson,
      _.fromJson[DiscussionAction].left.map(Exception(_))
    ).build()

  val topicsToReview: Var[DiscussionState] =
    Var(DiscussionState())


  val errorBanner =
    ErrorBanner()

  val submitNewTopic: Observer[DiscussionAction] = Observer {
    discussion =>
      discussion match
        case add: DiscussionAction.Add =>
          if (add.facilitator.unwrap.trim.length < 2)
            errorBanner.error.set(Some("User name too short. Tell us who you are!"))
          else
            errorBanner.error.set(None)
            topicUpdates.sendOne(discussion)
        case _ => ()
  }

  val name = getOrCreatePersistedName()
  val liveTopicSubmissionAndVoting =
    div(
      cls := "PageContainer",
      topicUpdates.connect,
      topicUpdates.received --> Observer {
        (event: DiscussionAction) =>
          println("Websocket Event: " + event)
          topicsToReview.update(existing =>
            existing(event)
          )
      },
      errorBanner.component,
      NameBadge(name),
      TopicSubmission(submitNewTopic, name.signal, errorBanner.error.toObserver),
      DiscussionsToReview(topicsToReview.signal, name.signal, topicUpdates.sendOne),
    )

  val app = {
    liveTopicSubmissionAndVoting
//    ScheduleView()
  }

  render(container, app)