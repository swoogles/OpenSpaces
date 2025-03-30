package co.wtf.openspaces


import co.wtf.openspaces.VotePosition.NotInterested
import zio.*
import zio.json.*
import zio.direct.*
import zio.http.*
import zio.http.codec.{Doc, HeaderCodec}
import zio.http.codec.HttpCodec.query
import zio.http.endpoint.Endpoint

import zio.http.ChannelEvent.{ExceptionCaught, Read, UserEvent, UserEventTriggered}

case class BackendSocketApp(
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
            val discussionAction = ZIO.fromEither(text.fromJson[DiscussionAction])
              .mapError(deserError => new Exception(s"Failed to deserialize: $deserError"))
              .run

            val actionResult = discussionDataStore.applyAction(discussionAction).run
            defer:
              val channels = connectedUsers.get.run
              println(s"Action result: \n $actionResult\nWill send to ${channels.size} connected users.")
              ZIO.foreachParDiscard(channels)(channel =>
                val fullJson = actionResult.toJsonPretty
                ZIO.debug(s"Sending discussion: $fullJson to $channel") *>
                  channel.send(Read(WebSocketFrame.text(fullJson))).ignore
              ).run
            .run

        //            .catchAll(ex => ZIO.debug("Failed to handle action: " + ex)) // TODO Does this screw up the socket connection if we let errors escape?

        case UserEventTriggered(UserEvent.HandshakeComplete) =>
          defer:
            connectedUsers.update(_ :+ channel).run
            val discussions = discussionDataStore.snapshot.run
            ZIO.foreachDiscard(discussions.data)((topic, discussion) =>
              val content: DiscussionActionConfirmed = DiscussionActionConfirmed.AddResult(discussion)
              channel.send(Read(WebSocketFrame.text(content.toJson)))
            ).run

            ZIO.when(false):
              defer:
                val action = discussionDataStore.randomDiscussionAction.run
                channel.send(Read(WebSocketFrame.text(action.toJson))).run
              .repeat(Schedule.spaced(1.seconds) && Schedule.forever)
            .forkDaemon.run

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
      Method.GET / "discussions" -> handler(socketApp.toResponse),
    )
    

object BackendSocketApp:
  val layer =
    ZLayer.fromZIO:
      defer:
        BackendSocketApp(
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
