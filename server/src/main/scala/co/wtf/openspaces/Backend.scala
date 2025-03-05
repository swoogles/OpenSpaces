package co.wtf.openspaces

import zio.{http, *}
import zio.Console.printLine
import zio.http.*
import zio.json.*

object Backend extends ZIOAppDefault {


  import zio._

  import zio.http.ChannelEvent.{ExceptionCaught, Read, UserEvent, UserEventTriggered}
  import zio.http._

  case class ApplicationState(connectedUsers: Ref[List[WebSocketChannel]], discussions: Ref[List[Discussion]]):

    val socketApp: WebSocketApp[Any] =
      Handler.webSocket { channel =>
        channel.receiveAll {
          case Read(WebSocketFrame.Text("end")) =>
            channel.shutdown

          // Echo the same message 10 times if it's not "foo" or "bar"
          case Read(WebSocketFrame.Text(text)) =>
            (for
              discussion <- ZIO.fromEither(text.fromJson[DiscussionAction])
                .mapError(deserError => new Exception(s"Failed to deserialize: $deserError"))
              _ <- discussion match
                case DiscussionAction.Delete(topic) =>
                  discussions.update(_.filterNot(_.topic == topic))
                case DiscussionAction.Add(discussion) =>
                  discussions.update(_ :+ discussion)
              _ <-
                for
                  channels <- connectedUsers.get
                  _ <-
                    ZIO.foreachDiscard(channels)( channel =>
                      ZIO.debug(s"Sending discussion: $discussion to $channel") *>
                      channel.send(Read(WebSocketFrame.text(discussion.toJson))).ignore
                    )
                yield ()
              _ <-
                channel
                  .send(Read(WebSocketFrame.text(s"echo $text")))
                  .repeatN(10)
                  .catchSomeCause { case cause =>
                    ZIO.logErrorCause(s"failed sending", cause)
                  }
            yield ())
              .catchAll(ex => ZIO.debug("Failed to echo: " + ex))

          // Send a "greeting" message to the client once the connection is established
          case UserEventTriggered(UserEvent.HandshakeComplete) =>
            for
              _ <- connectedUsers.update(_ :+ channel)
              discussions <- discussions.get
              _ <-
              ZIO.foreachDiscard(discussions)(discussion =>
              channel.send(Read(WebSocketFrame.text(DiscussionAction.Add(discussion).asInstanceOf[DiscussionAction].toJson)))
              )
              _ <- Console.printLine("Should send greetings")
            yield ()


          // Log when the channel is getting closed
          case Read(WebSocketFrame.Close(status, reason)) =>
            Console.printLine("Closing channel with status: " + status + " and reason: " + reason)

          // Print the exception if it's not a normal close
          case ExceptionCaught(cause) =>
            Console.printLine(s"Channel error!: ${cause.getMessage}")

          case other =>
            ZIO.debug("Other channel event: " + other)
        }
      }

    val voteSockets: WebSocketApp[Any] =
      Handler.webSocket { channel =>
        channel.receiveAll {
          case Read(WebSocketFrame.Text("end")) =>
            channel.shutdown

          // Echo the same message 10 times if it's not "foo" or "bar"
          case Read(WebSocketFrame.Text(text)) =>
            (for
              submittedDiscussion <- ZIO.fromEither(text.fromJson[Discussion])
                .mapError(deserError => new Exception(s"Failed to deserialize: $deserError"))
              updatedDiscussions <- discussions.updateAndGet(
                currentDiscussions =>
                      currentDiscussions.map {
                        discussion =>
                          if (discussion.topic == submittedDiscussion.topic)
                            discussion.copy(votes = discussion.votes + 1)
                          else
                            discussion
                      }
              )
              updatedDiscussion = updatedDiscussions.find(_.topic == submittedDiscussion.topic).get
              _ <-
                for
                  channels <- connectedUsers.get
                  _ <-
                    ZIO.foreachDiscard(channels)(channel =>
                      ZIO.debug(s"Sending discussion: $updatedDiscussion to $channel") *>
                        channel.send(Read(WebSocketFrame.text(DiscussionAction.Add(updatedDiscussion).asInstanceOf[DiscussionAction].toJson))).ignore
                    )
                yield ()
              _ <-
                channel
                  .send(Read(WebSocketFrame.text(s"echo $text")))
                  .repeatN(10)
                  .catchSomeCause { case cause =>
                    ZIO.logErrorCause(s"failed sending", cause)
                  }
            yield ())
              .catchAll(ex => ZIO.debug("Failed to echo: " + ex))


          // Print the exception if it's not a normal close
          case ExceptionCaught(cause) =>
            Console.printLine(s"Channel error!: ${cause.getMessage}")

          case other =>
            ZIO.debug("Other channel event: " + other)
        }
      }

    val socketRoutes =
      Routes(
        Method.GET / "discussions"          -> handler(socketApp.toResponse),
        Method.GET / "votes" -> handler(voteSockets.toResponse)
      )

  object ApplicationState:
    val layer =
      ZLayer.fromZIO:
        for {
          state <- Ref.make(List.empty[WebSocketChannel])
          topics <- Ref.make(List(Discussion("Default 1", 4, "system-hardcoded"), Discussion("Default 2", 0, "system-hardcoded")))
        } yield ApplicationState(state, topics)


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