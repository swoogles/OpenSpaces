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

private def getOrCreatePersistedName(): Var[String] =
  val name =
    try {
      val retrieved =
        localStorage
          .getItem("name")
      Option.when(retrieved != null && !retrieved.isBlank)(
        retrieved
      )
    }
    catch {
      case e: Exception =>
        println("Error retrieving existing name: " + e)
        None
    }
  Var(name.getOrElse(""))

private def BannerLogo() =
  div( width := "100%",
    img(cls := "LogoImg", src := "./wtf-web-nodate.jpg", role := "img")
  )

private def NameBadge(textVar: Var[String]) =
  div(cls := "Banner",
    img(cls := "LogoImg", src := "./wtf-web-nodate.jpg", role := "img"),
    div(
      span("Name:"),
      input(
        placeholder := "Enter your name",
        value <-- textVar,
        onInput.mapToValue --> textVar,
        textVar --> Observer {
          (value: String) =>
            localStorage.setItem("name", value)
        }
      ),
    )
  )

private def TopicSubmission(
                             submitEffect: Observer[Discussion],
                             name: StrictSignal[String],
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
            Discussion.apply(
              topicTitle, 
              name.now(), 
              Set(name.now())
            )
        ) --> submitEffect,
      "Submit"
    )
  )


private def DiscussionsToReview(
                                 topics: Signal[DiscussionState],
                                 name: StrictSignal[String],
                                 topicUpdates: DiscussionAction => Unit
                               ) =
  val localTopics = topics.map(_.data.values.toList)
  div(
    cls := "TopicsContainer",
    children <--
      localTopics
        .splitTransition(_.topic.unwrap)(
          (index, topic, signal, transition) =>
            div( cls := "TopicCard",
              transition.height,
                div( cls := "TopicBody",
                  div(
                    display := "flex",
                    justifyContent := "space-between",
                    h3(Random.nextInt(100).toString + topic.topic.unwrap),
                    if (List("bill", "emma").exists( admin =>  name.now().toLowerCase().contains(admin)))
                      button(
                        cls:="delete-topic",
                        color := "red",
                        border := "none", backgroundColor := "transparent", onClick --> Observer {
                          _ =>
                            topicUpdates(DiscussionAction.Delete(topic.topic))
                        },
                        "x"
                      )
                    else span()
                  ),
                  span(
                    cls := "VoteContainer",
                    p(topic.facilitator),
                    p("Votes ", child<--signal.map(_.votes), " "),

                    child <-- signal.map(
                      topicLive =>
                        if topicLive.interestedParties.contains(name.now()) then
                          button(
                            cls := "RemoveButton", onClick --> Observer {
                              _ =>
                                println("CLicked remove vote for: " + topicLive.topic)
                                topicUpdates(DiscussionAction.RemoveVote(topicLive.topic, name.now()))
                            },
                            "-"
                            //                    img(src := "./minus-icon.svg", role := "img") // TODO can we get a minus icon?
                          )
                        else
                          button(
                            cls := "AddButton", onClick --> Observer {
                              _ =>
                                topicUpdates(DiscussionAction.Vote(topicLive.topic, name.now()))
                            },
                            img(src := "./plus-icon.svg", role := "img")
                          ),
                    )
                  )
              )
            )
        )
  )


enum AppView:
  case Home
  case ScheduleView
  case SubmitTopic

case class ScheduleSlot(room: Room)

def DaySchedule(slots: Var[List[ScheduleSlot]]) =
  div(
    children <-- slots.signal.map {
      slots =>
        slots.map {
          slot =>
            div(
              slot.toString
            )
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

  val submitNewTopic: Observer[Discussion] = Observer {
    discussion =>
      if (discussion.facilitator.trim.length < 2)
        errorBanner.error.set(Some("User name too short. Tell us who you are!"))
      else
        errorBanner.error.set(None)
        topicUpdates.sendOne(DiscussionAction.Add(discussion))
  }

  val app = {
    val name = getOrCreatePersistedName()
    div(
      cls := "PageContainer",
      topicUpdates.connect,
      topicUpdates.received --> Observer {
        (event: DiscussionAction) =>
          println("From MY WS: " + event)
          topicsToReview.update(existing =>
            existing(event)
          )
      },
      EventStream.fromValue("blah") --> Observer {
        _ =>
          dom.window.setTimeout(
            () =>
              topicUpdates.sendOne(
                DiscussionAction.Rename(
                  Topic.parseOrDie("Continuous Deployment - A goal, or an asymptote?"),
                  Topic.parseOrDie("CD - Simpler title "))
              ),
            3000L
          )
          println()
      },
      errorBanner.component,
      NameBadge(name),
      TopicSubmission(submitNewTopic, name.signal, errorBanner.error.toObserver),
      DiscussionsToReview(topicsToReview.signal, name.signal, topicUpdates.sendOne),
    )
  }
  render(container, app)