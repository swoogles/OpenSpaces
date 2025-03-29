package co.wtf.openspaces

import co.wtf.openspaces.VotePosition.NotInterested
import zio.*
import zio.json.*
import zio.direct.*
import zio.http.*
import zio.http.codec.HttpCodec.query
import zio.http.endpoint.Endpoint

case class ClientId(value: String):
  override def toString: String = value

case class ClientSecret(value: String):
  override def toString: String = value

object Backend extends ZIOAppDefault {
  import zio.http.ChannelEvent.{ExceptionCaught, Read, UserEvent, UserEventTriggered}

  class DiscussionDataStore(discussionDatabase: Ref[DiscussionState],
                            glyphiconService: GlyphiconService):
    def snapshot =
      discussionDatabase.get

    def applyAction(discussionAction: DiscussionAction): UIO[DiscussionActionConfirmed] =
      discussionAction match
        case DiscussionAction.Add(topic, facilitator) =>
          for {
            randomIcon <- glyphiconService.getRandomIcon
            randomId <- Random.nextLong
            discussion = Discussion(
              topic,
              facilitator,
              Set(Feedback(facilitator, VotePosition.Interested)),
              TopicId(randomId),
              randomIcon,
              None
            )
            res <- discussionDatabase.updateAndGet(s => s(discussion))
          } yield DiscussionActionConfirmed.AddResult(discussion)

        case other =>
          defer:
            val confirmedAction = DiscussionActionConfirmed.fromDiscussionAction(other)
            discussionDatabase.updateAndGet(s => s(confirmedAction)).run
            confirmedAction

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
              DiscussionTopics.randomTopic.run,
              person,
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
                val newTopic = DiscussionTopics.randomTopic.run
                DiscussionAction.Rename(id, newTopic)
            }
        applyAction(action).as(action).run

  object DiscussionDataStore:
    val layer =
      ZLayer.fromZIO:
        defer:
          DiscussionDataStore(
            Ref.make(
              DiscussionState.example
            ).run,
            ZIO.service[GlyphiconService].run
          )


  case class ApplicationState(
                               connectedUsers: Ref[List[WebSocketChannel]],
                               discussionDataStore: DiscussionDataStore,
                               glyphiconService: GlyphiconService,
                               clientId: ClientId,
                               clientSecret: ClientSecret,
                               client: Client
                             ):

    val socketApp: WebSocketApp[Any] =
      Handler.webSocket { channel =>
        channel.receiveAll {
          case Read(WebSocketFrame.Text(text)) =>
            defer:
              println("Raw text: "+ text)
              val discussionAction = ZIO.fromEither(text.fromJson[DiscussionAction])
                .mapError(deserError => new Exception(s"Failed to deserialize: $deserError"))
                .run

              println("Going to do action: " + discussionAction)


              val actionResult = discussionDataStore.applyAction(discussionAction).run
              println("Action result: " + actionResult)
              defer:
                val channels = connectedUsers.get.run
                ZIO.foreachParDiscard(channels)( channel =>
                  val fullJson = actionResult.toJsonPretty
                  ZIO.debug(s"Sending discussion: $fullJson to $channel") *>
                    channel.send(Read(WebSocketFrame.text(fullJson))).ignore
                ).run
              .run

            .catchAll(ex => ZIO.debug("Failed to echo: " + ex))

          case UserEventTriggered(UserEvent.HandshakeComplete) =>
            defer:
              connectedUsers.update(_ :+ channel).run
              val discussions = discussionDataStore.snapshot.run
              println("Current discussion size: " + discussions.data.size)
              ZIO.foreachDiscard(discussions.data)((topic, discussion) =>
                // TODO Make sure these are idompotent
                channel.send(Read(WebSocketFrame.text(DiscussionActionConfirmed.AddResult(discussion).asInstanceOf[DiscussionActionConfirmed].toJson)))
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

    val  callback =
      Endpoint(
        Method.GET /
          "callback"
      )
        .query(
          query[String]("code")
        )
        .out[String]

    val callbackRoute =
      callback.implement { (code) =>
        (for {
          _ <- ZIO.debug("callback! Code: " + code)
          /*
           TODO Scala zio-http version of this code:
           app.get("/callback", (req, res) => {
              axios.post("https://github.com/login/oauth/access_token", {
                  client_id: "33a703d019e0d23730ea",
                  client_secret: "6f52b07d679f6955317a4fe7983d4b3b6cb0aa2e",
                  code: req.query.code
              }, {
                  headers: {
                      Accept: "application/json"
                  }
              }).then((result) => {
                  console.log(result.data.access_token)
                  res.send("you are authorized " + result.data.access_token)
              }).catch((err) => {
                  console.log(err);
              })
          })
           */
          res    <- client
            .url(
              URL.decode("https://github.com/login/oauth")
                .getOrElse(???)
                .addQueryParam("client_id", clientId.value)
                .addQueryParam("client_secret", clientSecret.value)
                .addQueryParam("code", code)
            )
            .get("/access_token")
          data   <- res.body.asString
          _      <- Console.printLine(data)
        } yield "Hi there. Should have done some more callback stuff"
        ).orDie
      }

    val socketRoutes =
      Routes(
        callbackRoute,
        Method.GET / "discussions"          -> handler(socketApp.toResponse),
        // TODO Build this URL way sooner
        Method.GET / "auth" -> handler(Response.redirect(URL.decode(s"https://github.com/login/oauth/authorize?client_id=$clientId").getOrElse(???))),
      )

  object ApplicationState:
    val layer =
      ZLayer.fromZIO:
        defer:
          ApplicationState(
            Ref.make(List.empty[WebSocketChannel]).run,
            ZIO.service[DiscussionDataStore].run,
            ZIO.service[GlyphiconService].run,
            System.env("GITHUB_CLIENT_ID")
              .someOrFail(new Exception("No GITHUB_CLIENT_ID found in environment"))
              .orDie
              .map(ClientId(_))
              .run
            ,
            System.env("GITHUB_CLIENT_SECRET")
              .someOrFail(new Exception("No GITHUB_CLIENT_SECRET found in environment"))
              .orDie
              .map(ClientSecret(_))
              .run,
            ZIO.service[Client].run
          )



  override def run =
    val port = sys.env.getOrElse("PORT", throw new IllegalStateException("No value found for $PORT")).toInt
    defer:
      val statefulRoutes = ZIO.serviceWith[ApplicationState](_.socketRoutes).run

      Server.serve(statefulRoutes @@ Middleware.serveResources(Path.empty))
        .as("Just working around zio-direct limitation").run
    .provide(
      Server.defaultWith(_.port(port)),
      ApplicationState.layer,
      DiscussionDataStore.layer,
      GlyphiconService.live,
      Client.default,
      Scope.default
    )
}