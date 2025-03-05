package co.wtf.openspaces

import co.wtf.openspaces.{Discussion, Room, ScheduleSlot}
import com.raquo.laminar.api.L.{*, given}
import io.laminext.websocket.WebSocket
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
      onClick.mapTo(textVar.now()).map(topicTitle => Discussion.apply(topicTitle, 0)) --> submitEffect,
      "Submit topic"
    )
  )


private def DiscussionsToReview(topics: Signal[List[Discussion]]) =
  val topicUpdates = WebSocket.url("/votes").string.build()
  div(
    topicUpdates.connect,
    children <-- topics.map {
      topics =>
        topics.map {
          topic =>
            div(
              topic.toString,
              button(
                onClick --> Observer {
                  _ =>
                    topicUpdates.sendOne(topic.toJson)
                },
                "+1"
              )
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
      div(
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
        TopicSubmission(submitNewTopic),
        DiscussionsToReview(topicsToReview.signal),
      )
    }
    render(container, app)