package co.wtf.openspaces

import zio.*
import zio.direct.*
import zio.http.*
import zio.http.ChannelEvent.{
  ExceptionCaught,
  Read,
  UserEvent,
  UserEventTriggered,
}
import zio.json.*

case class OpenSpacesServerChannel(
  channel: WebSocketChannel,
):

  def send(message: DiscussionActionConfirmed): ZIO[Any, Throwable, Unit] =
    channel.send(ChannelEvent.Read(WebSocketFrame.text(message.toJson)))


case class BackendSocketApp(
  connectedUsers: Ref[List[WebSocketChannel]],
  discussionDataStore: DiscussionDataStore,
  authenticatedTicketService: AuthenticatedTicketService):

  def handleTicket(ticket: Ticket, channel: WebSocketChannel): ZIO[Any, Throwable, Unit] =
    val openSpacesServerChannel = OpenSpacesServerChannel(channel)

    defer:
      authenticatedTicketService
        .use(ticket)
        .tapError(e =>
          ZIO.debug(s"Error processing ticket: $e"),
        )
        .mapError(new Exception(_))
        .run
      connectedUsers
        .updateAndGet(_ :+ channel)
        .run
      val discussions = discussionDataStore.snapshot.run
      ZIO
        .foreachDiscard(discussions.data.values)(
            discussion =>
            openSpacesServerChannel
              .send(
                DiscussionActionConfirmed.AddResult(
                  discussion,
                )
              ),
        ).run

  def startSpawningRandomActions(channel: WebSocketChannel) =
    ZIO
      .when(true):
        defer:
          val action =
            discussionDataStore.randomDiscussionAction.run
          channel
            .send(
              Read(WebSocketFrame.text(action.toJson)),
            )
            .run
        .repeat(
          Schedule.spaced(
            500.millis,
            // 1.seconds,
          ) && Schedule.forever,
        )
      .forkDaemon

  private def handleTicketedAction(
    channel: WebSocketChannel,
    discussionAction: DiscussionAction
  ): ZIO[Any, Throwable, Unit] =
    defer:
      val actionResult = discussionDataStore
        .applyAction(discussionAction)
        .run
      defer:
        val channels = connectedUsers.get.run
        println(
          s"Action result: \n $actionResult\nWill send to ${channels.size} connected users.",
        )
        ZIO
          .foreachParDiscard(channels)(channel =>
            val fullJson = actionResult.toJsonPretty
            channel
              .send(
                Read(WebSocketFrame.text(fullJson)),
              )
              .ignore,
          )
          .run
      .run

  private def handleUnticketedAction(
    channel: WebSocketChannel,
    discussionAction: DiscussionAction
  ): ZIO[Any, Throwable, Unit] =
    OpenSpacesServerChannel(channel)
      .send(
        DiscussionActionConfirmed.Rejected(
          discussionAction,
        )
      ).debug("unticketed. Sending back")
      .ignore

  val socketApp: WebSocketApp[Any] =
    Handler.webSocket { channel =>
      channel.receiveAll {
        case Read(WebSocketFrame.Text(text)) =>
          println("Received message: " + text)
          text.fromJson[WebSocketMessage] match
            case Left(value) =>
              ZIO.debug(s"Server received invalid message: $value")
            case Right(value) =>
              value match
                case ticket: Ticket =>
                  defer:
                    handleTicket(ticket, channel).debug("handleTicket").run
                    startSpawningRandomActions(channel).debug("startSpawningRandomActions").run
                case discussionAction: DiscussionAction =>
                  if (connectedUsers.get.run.contains(channel))
                    handleTicketedAction(channel, discussionAction)
                  else
                    handleUnticketedAction(channel, discussionAction)

        case UserEventTriggered(UserEvent.HandshakeComplete) =>
          ZIO.debug("Server Handshake complete. Waiting for a valid ticket before sending data.")

        case Read(WebSocketFrame.Close(status, reason)) =>
          Console.printLine(
            "Closing channel with status: " + status + " and reason: " + reason,
          )

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
          ZIO.service[AuthenticatedTicketService].run,
        )
