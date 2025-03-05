import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

case class Discussion(topic: String)

def TopicSubmission(submitEffect: Observer[Discussion]) =
  val intBus = new EventBus[Int]
  val textVar = Var("")
  div(
    input(
      onClick.mapTo(1) --> intBus,
      value <-- textVar,
      onInput.mapToValue --> textVar,
      onMouseOver --> { ev => println(ev) },
      onChange --> { _ => println("committed") },
      onBlur.mapTo("blur") --> Observer {blurValue => println(blurValue)}
    ),
    button(
      onClick.mapTo(textVar.now()).map(Discussion.apply) --> submitEffect,
      "Submit"
    )
  )

object Main extends App:
    lazy val container = dom.document.getElementById("app")
    import io.laminext.websocket._


    val ws = WebSocket.url("wss://echo.websocket.org").string.build()

    val topicsToReview: Var[List[Discussion]] =
      Var(List(Discussion("Scala 3")))

    val submitNewTopic: Observer[Discussion] = Observer {
      discussion =>
        ws.sendOne(discussion.toString) // TODO Json
    }

    val app = {
      println("should have a ws?")
      div(
        ws.connect,
        ws.received --> Observer {
          event =>
          println("From WS: " + event)
        } ,
        TopicSubmission(submitNewTopic),
        button(
          onClick.mapTo("boop_" + scala.util.Random.nextInt()) --> ws.send,
          "Click me better"
        ),
      )
    }
    println("Doing app now???")
    render(container, app)