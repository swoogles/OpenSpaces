import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

object Main extends App:
    lazy val container = dom.document.getElementById("app")
    import io.laminext.websocket._

    val ws = WebSocket.url("wss://echo.websocket.org").string.build()
    val app = {
      println("should have a ws?")
      div(

        ws.connect,
        ws.received --> Observer {
          event =>
          println("From WS: " + event)
        } ,
        button(
          onClick --> Observer {
            clickyEvent =>
              println("Should be sending a message")
              ws.sendOne("a message")
          },
          "Click me"
        ),
        button(
          onClick.mapTo("boop") --> ws.send,
          "Click me better"
        ),
        "Replaced"
      )
    }
    println("Doing app now???")
    render(container, app)