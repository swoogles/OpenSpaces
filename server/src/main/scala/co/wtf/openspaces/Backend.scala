package co.wtf.openspaces

import zio.*
import zio.Console.printLine
import zio.http.*
import zio.json.*

val randomDiscussion =
  for
    id <- Random.nextIntBounded(10)
  yield Discussion("Blah_" + id)

object Backend extends ZIOAppDefault {


  import zio._

  import zio.http.ChannelEvent.{ExceptionCaught, Read, UserEvent, UserEventTriggered}
  import zio.http._
  import zio.http.codec.PathCodec.string

  case class ApplicationState(channels: Ref[List[WebSocketChannel]]):

    val socketApp: WebSocketApp[Any] =
      Handler.webSocket { channel =>
        channel.receiveAll {
          case Read(WebSocketFrame.Text("end")) =>
            channel.shutdown

          // Send a "bar" if the client sends a "foo"
          case Read(WebSocketFrame.Text("foo")) =>
            channel.send(Read(WebSocketFrame.text("bar")))

          // Send a "foo" if the client sends a "bar"
          case Read(WebSocketFrame.Text("bar")) =>
            channel.send(Read(WebSocketFrame.text("foo")))

          // Echo the same message 10 times if it's not "foo" or "bar"
          case Read(WebSocketFrame.Text(text)) =>
            channel
              .send(Read(WebSocketFrame.text(s"echo $text")))
              .repeatN(10)
              .catchSomeCause { case cause =>
                ZIO.logErrorCause(s"failed sending", cause)
              }

          // Send a "greeting" message to the client once the connection is established
          case UserEventTriggered(UserEvent.HandshakeComplete) =>
            channels.update(_ :+ channel) *>
            Console.printLine("Should send greetings")


          // Log when the channel is getting closed
          case Read(WebSocketFrame.Close(status, reason)) =>
            Console.printLine("Closing channel with status: " + status + " and reason: " + reason)

          // Print the exception if it's not a normal close
          case ExceptionCaught(cause) =>
            Console.printLine(s"Channel error!: ${cause.getMessage}")

          case _ =>
            ZIO.unit
        }
      }

    val socketRoutes =
      Routes(
        Method.GET / "discussions"          -> handler(socketApp.toResponse)
      )

  object ApplicationState:
    val layer =
      ZLayer.fromZIO:
        for {
          state <- Ref.make(List.empty[WebSocketChannel])
          _ <-
            (for {
              discussion <- randomDiscussion
              channels <- state.get
              _ <-
                ZIO.foreachDiscard(channels)( channel =>
                  channel.send(Read(WebSocketFrame.text(discussion.toJson)))
                )
            } yield ()).repeat(Schedule.spaced(5.seconds) && Schedule.forever).forkDaemon
        } yield ApplicationState(state)


  /**
   * Creates an HTTP app that only serves static files from resources via
   * "/static". For paths other than the resources directory, see
   * [[zio.http.Middleware.serveDirectory]].
   */
//  val routes = socketRoutes

  override def run =
    (for {
      statefulRoutes <- ZIO.serviceWith[ApplicationState](_.socketRoutes)
      _ <- Server.serve(statefulRoutes @@ Middleware.serveResources(Path.empty))
    } yield ())
    .provide(
      Server.default,
      ApplicationState.layer
    )
}