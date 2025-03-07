package co.wtf.openspaces

import zio.*
import zio.json.*
import zio.direct.*

object Backend extends ZIOAppDefault {
  import zio.http.ChannelEvent.{ExceptionCaught, Read, UserEvent, UserEventTriggered}
  import zio.http._

  class DiscussionDataStore(discussionDatabase: Ref[DiscussionState]):
    def snapshot =
      discussionDatabase.get

    def applyAction(discussionAction: DiscussionAction): UIO[DiscussionState] =
      discussionDatabase.updateAndGet(s => s(discussionAction))

  object DiscussionDataStore:
    val layer =
      ZLayer.fromZIO:
        defer:
          val topics = Ref.make(
            DiscussionState(
              Discussion.example1,
              Discussion.example2
            )
          ).run
          DiscussionDataStore(topics)


  case class ApplicationState(connectedUsers: Ref[List[WebSocketChannel]], discussionDataStore: DiscussionDataStore):

    val socketApp: WebSocketApp[Any] =
      Handler.webSocket { channel =>
        channel.receiveAll {
          case Read(WebSocketFrame.Text(text)) =>
            defer:
              val discussionAction = ZIO.fromEither(text.fromJson[DiscussionAction])
                .mapError(deserError => new Exception(s"Failed to deserialize: $deserError"))
                .run


              val updatedDiscussions = discussionDataStore.applyAction(discussionAction).run
              println("updatedDiscussion: " + updatedDiscussions)
              defer:
                val channels = connectedUsers.get.run
                ZIO.foreachDiscard(channels)( channel =>
                  val fullJson = discussionAction.toJsonPretty
                  ZIO.debug(s"Sending discussion: $fullJson to $channel") *>
                    channel.send(Read(WebSocketFrame.text(fullJson))).ignore
                ).run
              .run

            .catchAll(ex => ZIO.debug("Failed to echo: " + ex))

          case UserEventTriggered(UserEvent.HandshakeComplete) =>
            defer:
              connectedUsers.update(_ :+ channel).run
              val discussions = discussionDataStore.snapshot.run
              ZIO.foreachDiscard(discussions.data)((topic, discussion) =>
                channel.send(Read(WebSocketFrame.text(DiscussionAction.Add(discussion).asInstanceOf[DiscussionAction].toJson)))
              ).run
              Console.printLine("Should send greetings").run

          case Read(WebSocketFrame.Close(status, reason)) =>
            Console.printLine("Closing channel with status: " + status + " and reason: " + reason)

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
        defer:
          val discussionDataStore = ZIO.service[DiscussionDataStore].run
          val state = Ref.make(List.empty[WebSocketChannel]).run
          ApplicationState(state, discussionDataStore)



  override def run =
    defer:
      val statefulRoutes = ZIO.serviceWith[ApplicationState](_.socketRoutes).run
      Server.serve(statefulRoutes @@ Middleware.serveResources(Path.empty)).as("Just working around zio-direct limitation").run
    .provide(
      Server.default,
      ApplicationState.layer,
      DiscussionDataStore.layer
    )
}