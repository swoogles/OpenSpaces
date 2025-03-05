package co.wtf.openspaces

import co.wtf.openspaces.{Discussion, Room, ScheduleSlot}
import com.raquo.laminar.api.L.{*, given}
import io.laminext.websocket.WebSocket
import org.scalajs.dom
import zio.json.*
import org.scalajs.dom.window

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
  div(
      div(
        input(
          value <-- textVar,
          onInput.mapToValue --> textVar,
          textVar --> Observer {
            (value: String) =>
              localStorage.setItem("name", value)
              println("Name: " + value)
          }
        ),
      )
  )

private def TopicSubmission(submitEffect: Observer[Discussion], name: StrictSignal[String]) =

  val intBus = new EventBus[Int]
  val textVar = Var("")
  div( cls := "Flex",
    span(
      "Topic: ",
      input(
        onClick.mapTo(1) --> intBus,
        value <-- textVar,
        onInput.mapToValue --> textVar,
        onMouseOver --> { ev => println(ev) },
        onChange --> { _ => println("committed") },
        onBlur.mapTo("blur") --> Observer {blurValue => println(blurValue)}
      )
    ),
    button(
      onClick.mapTo(textVar.now()).map(topicTitle => Discussion.apply(topicTitle, 0, name.now())) --> submitEffect,
      "Submit topic"
    )
  )


private def DiscussionsToReview(topics: Signal[List[Discussion]]) =
  val topicUpdates = WebSocket.url("/votes").string.build()
  div(
    cls := "TopicsContainer", topicUpdates.connect,
    children <-- topics.map {
      topics =>
        topics.sortBy(_.votes).sortWith(_.votes > _.votes).map {
          topic =>
            div( cls := "TopicCard",
              div( cls := "TopicBody", h3(topic.topic), span(cls := "VoteContainer",p(topic.facilitator), p("Votes ", topic.votes, " "),
              button(
                cls := "AddButton", onClick --> Observer {
                  _ =>
                    topicUpdates.sendOne(topic.toJson)
                },
                img(src := "./plus-icon.svg", role := "img")
              )))
            )
        }
    }
  )


enum AppView:
  case Home
  case ScheduleView
  case SubmitTopic

enum Room:
  case King
  case ArtGallery
  case Hawk
  case DanceHall

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

object FrontEnd extends App:
    lazy val container = dom.document.getElementById("app")
    import io.laminext.websocket.*

    val topicUpdates = WebSocket.url("/discussions").string.build()

    val topicsToReview: Var[List[Discussion]] =
      Var(List.empty)

    val submitNewTopic: Observer[Discussion] = Observer {
      discussion =>
        topicUpdates.sendOne(discussion.toJson) // TODO Json
    }

    val app = {
      val name = getOrCreatePersistedName()
      div(
        cls := "PageContainer",
        topicUpdates.connect,
        topicUpdates.received --> Observer {
          (event: String) =>
            println("From MY WS: " + event)
            topicsToReview.update(existing =>
              event.fromJson[Discussion] match
                case Left(value) =>
                  println("Uh oh, bad discussion sent from server: " + value)
                  existing
                case Right(value) =>
                  if (existing.exists(_.topic == value.topic))
                    existing.map {
                      discussion =>
                        if discussion.topic == value.topic then
                          value
                        else
                          discussion
                    }
                  else
                    value :: existing
            )
        },
        BannerLogo(),
        NameBadge(name),
        TopicSubmission(submitNewTopic, name.signal),
        DiscussionsToReview(topicsToReview.signal),
      )
    }
    render(container, app)