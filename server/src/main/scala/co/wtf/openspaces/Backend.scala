package co.wtf.openspaces

import zio.*
import zio.json.*

object Backend extends ZIOAppDefault {


  import zio._

  import zio.http.ChannelEvent.{ExceptionCaught, Read, UserEvent, UserEventTriggered}
  import zio.http._

  class DiscussionDataStore(discussionDatabase: Ref[List[Discussion]]):
    def snapshot =
      discussionDatabase.get

    def applyAction(discussionAction: DiscussionAction) =
      discussionDatabase.updateAndGet(
        currentDiscussions =>
          discussionAction match
            case DiscussionAction.Delete(topic) =>
              currentDiscussions.filterNot(_.topic == topic)
            case DiscussionAction.Add(discussion) =>
              currentDiscussions :+ discussion // Only add if new topic title
            case DiscussionAction.Vote(topic, voter) =>
              currentDiscussions.map {
                discussion =>
                  if (discussion.topic == topic)
                    println("Bumping the count")
                    discussion.copy(interestedParties = discussion.interestedParties + voter)
                  else
                    discussion
              }
            case DiscussionAction.RemoveVote(topic, voter) =>
              currentDiscussions.map {
                discussion =>
                  if (discussion.topic == topic)
                    println("Removing the count")
                    discussion.copy(interestedParties = discussion.interestedParties - voter)
                  else
                    discussion
              }
      )

  object DiscussionDataStore:
    val layer =
      ZLayer.fromZIO:
        for {
          topics <- Ref.make(List(Discussion("Continuous Deployment - A goal, or an asymptote?", "Bill", Set("Bill")), Discussion("Managing emotional energy on the job", "Emma", Set("Emma"))))
        } yield DiscussionDataStore(topics)


  case class ApplicationState(connectedUsers: Ref[List[WebSocketChannel]], discussionDataStore: DiscussionDataStore):

    val socketApp: WebSocketApp[Any] =
      Handler.webSocket { channel =>
        channel.receiveAll {

          // Echo the same message 10 times if it's not "foo" or "bar"
          case Read(WebSocketFrame.Text(text)) =>
            (for
              _ <- ZIO.debug("raw Json: " + text)
              discussionAction <- ZIO.fromEither(text.fromJson[DiscussionAction])
                .mapError(deserError => new Exception(s"Failed to deserialize: $deserError"))


              updatedDiscussions <- discussionDataStore.applyAction(discussionAction)
              _ <-
                for
                  channels <- connectedUsers.get
                  _ <-
                    ZIO.foreachDiscard(channels)( channel =>
                      val fullJson = discussionAction.toJson
                      ZIO.debug(s"Sending discussion: $fullJson to $channel") *>
                      channel.send(Read(WebSocketFrame.text(fullJson))).ignore
                    )
                yield ()
            yield ())
              .catchAll(ex => ZIO.debug("Failed to echo: " + ex))

          // Send a "greeting" message to the client once the connection is established
          case UserEventTriggered(UserEvent.HandshakeComplete) =>
            for
              _ <- connectedUsers.update(_ :+ channel)
              discussions <- discussionDataStore.snapshot
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

    val socketRoutes =
      Routes(
        Method.GET / "discussions"          -> handler(socketApp.toResponse),
      )

  object ApplicationState:
    val layer =
      ZLayer.fromZIO:
        for {
          discussionDataStore <- ZIO.service[DiscussionDataStore]
          state <- Ref.make(List.empty[WebSocketChannel])
          topics <- Ref.make(List(Discussion("Continuous Deployment - A goal, or an asymptote?", "Bill", Set("Bill")), Discussion("Managing emotional energy on the job", "Emma", Set("Emma"))))
        } yield ApplicationState(state, discussionDataStore)


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
      ApplicationState.layer,
      DiscussionDataStore.layer
    )
}