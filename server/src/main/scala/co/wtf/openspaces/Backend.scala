package co.wtf.openspaces

import co.wtf.openspaces.VotePosition.NotInterested
import zio.*
import zio.json.*
import zio.direct.*
import zio.http.*


object Backend extends ZIOAppDefault {
  import zio.http.ChannelEvent.{ExceptionCaught, Read, UserEvent, UserEventTriggered}

  class DiscussionDataStore(discussionDatabase: Ref[DiscussionState]):
    def snapshot =
      discussionDatabase.get

    def applyAction(discussionAction: DiscussionAction): UIO[DiscussionState] =
      discussionDatabase.updateAndGet(s => s(discussionAction))

    private def randomExistingTopicId =
      defer:
        val data = snapshot.run
        val idx = Random.nextIntBounded(data.data.keys.toList.length).run
        data.data.keys.toList(idx)


    def randomDiscussionAction =
      defer:
        val actionIdx = Random.nextIntBounded(5).run
        val noCurrentItems = snapshot.run.data.keys.toList.isEmpty
        val addNewDiscussion =
          val person = Person("RandomPerson - " + Random.nextIntBounded(20).run)
          DiscussionAction.Add(
            Discussion(
              Topic.parseOrDie(DiscussionTopics.randomTopic.run),
              person,
              Set(Feedback(person, VotePosition.Interested)),
              TopicId(Random.nextLongBounded(20).run)
            )
          )

        val action =
          if (noCurrentItems)
            addNewDiscussion
          else
            actionIdx match {
              case 0 =>
                addNewDiscussion
              case 1 =>
                val id = randomExistingTopicId.run
                DiscussionAction.Delete(id)
              case 2 =>
                val id = randomExistingTopicId.run
                val person = Person("RandomPerson - " + Random.nextIntBounded(20).run)
                DiscussionAction.Vote(id, Feedback(person, VotePosition.Interested))

              case 3 =>
                val id = randomExistingTopicId.run
                // TODO Ensure existing person that has voted for topic, unless it's got 0 votes
                val person = Person("RandomPerson - " + Random.nextIntBounded(20).run)
                DiscussionAction.RemoveVote(id, person)
              case 4 =>
                val id = randomExistingTopicId.run
                val newTopic = Topic.parseOrDie(DiscussionTopics.randomTopic.run)
                DiscussionAction.Rename(id, newTopic)
            }
        applyAction(action).as(action).run

  object DiscussionDataStore:
    val layer =
      ZLayer.fromZIO:
        defer:
          DiscussionDataStore(
            Ref.make(
              DiscussionState(
                Discussion.example1,
                Discussion.example2,
                Discussion.example3,
                Discussion.example4,
                Discussion.example5,
                Discussion.example6
              )
            ).run
          )


  case class ApplicationState(
                               connectedUsers: Ref[List[WebSocketChannel]],
                               discussionDataStore: DiscussionDataStore
                             ):

    val socketApp: WebSocketApp[Any] =
      Handler.webSocket { channel =>
        channel.receiveAll {
          case Read(WebSocketFrame.Text(text)) =>
            defer:
              val discussionAction = ZIO.fromEither(text.fromJson[DiscussionAction])
                .mapError(deserError => new Exception(s"Failed to deserialize: $deserError"))
                .run


              val updatedDiscussions = discussionDataStore.applyAction(discussionAction).run
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

              ZIO.when(false):
                defer:
                  val action = discussionDataStore.randomDiscussionAction.run
                  channel.send(Read(WebSocketFrame.text(action.toJson))).run
                .repeat(Schedule.spaced(1.seconds) && Schedule.forever)
              .forkDaemon.run

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
          ApplicationState(
            Ref.make(List.empty[WebSocketChannel]).run,
            ZIO.service[DiscussionDataStore].run
          )



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