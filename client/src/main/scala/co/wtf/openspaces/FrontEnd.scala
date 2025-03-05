package co.wtf.openspaces

import co.wtf.openspaces.{Discussion, Room, ScheduleSlot}
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import zio.json.*

private def TopicSubmission(submitEffect: Observer[Discussion]) =
  val intBus = new EventBus[Int]
  val textVar = Var("")
  div(
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
      onClick.mapTo(textVar.now()).map(Discussion.apply) --> submitEffect,
      "Submit topic"
    )
  )


private def DiscussionsToReview(topics: Signal[List[Discussion]]) =
  div(
    children <-- topics.map {
      topics =>
        topics.map {
          topic =>
            div(
              topic.toString
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


    val ws = WebSocket.url("wss://echo.websocket.org").string.build()

    val topicsToReview: Var[List[Discussion]] =
      Var(List(Discussion("Scala 3")))

    val submitNewTopic: Observer[Discussion] = Observer {
      discussion =>
        ws.sendOne(discussion.toJson) // TODO Json
    }

    val app = {
      div(
        ws.connect,
        ws.received --> Observer {
          (event: String) =>
            println("From WS: " + event)
            topicsToReview.update(existing =>
              event.fromJson[Discussion] match
                case Left(value) =>
                  println("Uh oh, bad discussion sent from server: " + value)
                  existing
                case Right(value) => existing :+ value
            )
        } ,
        TopicSubmission(submitNewTopic),
        DiscussionsToReview(topicsToReview.signal),
        button(
          onClick.mapTo("boop_" + scala.util.Random.nextInt()) --> ws.send,
          "Click me better"
        ),
      )
    }
    render(container, app)