package co.wtf.openspaces

import zio.*
import zio.direct.*
import zio.http.*
import zio.json.*

case class DiscussionService(
  connectedUsers: Ref[List[OpenSpacesServerChannel]],
  discussionDataStore: DiscussionDataStore,
  authenticatedTicketService: AuthenticatedTicketService):

  def handleMessage(
    message: WebSocketMessage,
    channel: OpenSpacesServerChannel,
  ): ZIO[Any, Throwable, Unit] =
    message match
      case ticket: Ticket =>
        handleTicket(ticket, channel)
      case discussionAction: DiscussionAction =>
        defer:
          if (connectedUsers.get.run.contains(channel)) {
            handleTicketedAction(channel, discussionAction).run
          }
          else {
            handleUnticketedAction(channel, discussionAction).run
          }

  def randomDiscussionAction
    : ZIO[Any, Throwable, DiscussionActionConfirmed] =
    discussionDataStore.randomDiscussionAction

  private def handleTicket(
    ticket: Ticket,
    channel: OpenSpacesServerChannel,
  ): ZIO[Any, Throwable, Unit] =
    defer:
      authenticatedTicketService
        .use(ticket)
        .tapError(e => ZIO.unit)
        .mapError(new Exception(_))
        .run
      connectedUsers
        .updateAndGet(_ :+ channel)
        .run
      val discussions = discussionDataStore.snapshot.run
      ZIO
        .foreachDiscard(discussions.data.values)(discussion =>
          channel
            .send(
              DiscussionActionConfirmed.AddResult(
                discussion,
              ),
            ),
        )
        .run

  private def handleTicketedAction(
    channel: OpenSpacesServerChannel,
    discussionAction: DiscussionAction,
  ): ZIO[Any, Throwable, Unit] =
    defer:
      val actionResult = discussionDataStore
        .applyAction(discussionAction)
        .run
      defer:
        val channels = connectedUsers.get.run
        ZIO
          .foreachParDiscard(channels)(channel =>
            channel
              .send(
                actionResult,
              )
              .ignore,
          )
          .run
      .run

  private def handleUnticketedAction(
    channel: OpenSpacesServerChannel,
    discussionAction: DiscussionAction,
  ): ZIO[Any, Throwable, Unit] =
    channel
      .send(
        DiscussionActionConfirmed.Rejected(
          discussionAction,
        ),
      )
      .ignore

object DiscussionService:
  val layer =
    ZLayer.fromZIO:
      defer:
        DiscussionService(
          Ref.make(List.empty[OpenSpacesServerChannel]).run,
          ZIO.service[DiscussionDataStore].run,
          ZIO.service[AuthenticatedTicketService].run,
        )
