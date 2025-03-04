import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

object Main extends App:
    lazy val container = dom.document.getElementById("app")
    val app =
      div("Replaced")
    render(container, app)