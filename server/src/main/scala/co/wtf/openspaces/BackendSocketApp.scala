package co.wtf.openspaces


import zio.*
import zio.direct.*
import zio.http.*
import zio.http.ChannelEvent.{ExceptionCaught, Read, UserEvent, UserEventTriggered}
import zio.json.*

case class BackendSocketApp(
                             connectedUsers: Ref[List[WebSocketChannel]],
                             discussionDataStore: DiscussionDataStore,
                             authenticatedTicketService: AuthenticatedTicketService
                           ):

  val socketApp: WebSocketApp[Any] =
    Handler.webSocket { channel =>
      channel.receiveAll {
        case Read(WebSocketFrame.Text(text)) =>
          text.fromJson[WebSocketMessage] match
            case Left(value) => ???
            case Right(value) => value match
              case ticket: Ticket =>
                defer:
                  authenticatedTicketService.use(ticket).mapError(new Exception(_)).run
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
              case discussionAction: DiscussionAction =>
                defer:
                  if(connectedUsers.get.run.contains(channel)) {
                    val actionResult = discussionDataStore.applyAction(discussionAction).run
                    defer:
                      val channels = connectedUsers.get.run
                      println(s"Action result: \n $actionResult\nWill send to ${channels.size} connected users.")
                      ZIO.foreachParDiscard(channels)(channel =>
                        val fullJson = actionResult.toJsonPretty
                          channel.send(Read(WebSocketFrame.text(fullJson))).ignore
                      ).run
                    .run

                  } else {
                    ZIO.debug("Received action from an unticketed channel. Send back rejection").run
                    val content: DiscussionActionConfirmed = DiscussionActionConfirmed.Rejected(discussionAction)
                    channel.send(Read(WebSocketFrame.text(content.toJson))).ignore.run
                    // TODO Spit back the action to the user, so they can retry after being ticketed?
                  }

        case UserEventTriggered(UserEvent.HandshakeComplete) =>
          ZIO.unit

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
          ZIO.service[AuthenticatedTicketService].run
        )