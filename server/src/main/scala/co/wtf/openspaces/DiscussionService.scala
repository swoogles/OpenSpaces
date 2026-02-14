package co.wtf.openspaces

import co.wtf.openspaces.slack.SlackNotifier
import zio.*
import zio.direct.*
import zio.http.*
import zio.json.*

case class DiscussionService(
  connectedUsers: Ref[List[OpenSpacesServerChannel]],
  discussionStore: DiscussionStore,
  authenticatedTicketService: AuthenticatedTicketService,
  slackNotifier: SlackNotifier):

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

  /** Generate a random action and broadcast to all connected clients */
  def randomDiscussionAction: Task[DiscussionActionConfirmed] =
    for
      action <- discussionStore.randomDiscussionAction
      _ <- broadcastToAll(action)
      _ <- slackNotifier.notify(action, broadcastToAll)
    yield action

  /** Generate a random schedule-only action (move/swap/unschedule) and broadcast */
  def randomScheduleAction: Task[DiscussionActionConfirmed] =
    for
      action <- discussionStore.randomScheduleAction
      _ <- broadcastToAll(action)
      _ <- slackNotifier.notify(action, broadcastToAll)
    yield action

  /** Delete all topics using the standard delete logic (broadcasts to all clients) */
  def deleteAllTopics: Task[Int] =
    for
      state <- discussionStore.snapshot
      topicIds = state.data.keys.toList
      _ <- ZIO.foreachDiscard(topicIds) { topicId =>
        for
          result <- discussionStore.applyAction(DiscussionAction.Delete(topicId))
          _ <- broadcastToAll(result)
        yield ()
      }
    yield topicIds.size

  private def handleTicket(
    ticket: Ticket,
    channel: OpenSpacesServerChannel,
  ): ZIO[Any, Throwable, Unit] =
    defer:
      authenticatedTicketService
        .use(ticket)
        .mapError(new Exception(_))
        .run
      connectedUsers
        .updateAndGet(_ :+ channel)
        .run
      val discussions = discussionStore.snapshot.run
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

  private def broadcastToAll(message: DiscussionActionConfirmed): Task[Unit] =
    defer:
      // Update server's in-memory state (important for SlackThreadLinked)
      discussionStore.applyConfirmed(message).run
      val channels = connectedUsers.get.run
      ZIO
        .foreachParDiscard(channels)(channel =>
          channel.send(message).ignore,
        )
        .run

  /** Apply an action and broadcast to all clients + Slack. Used by scheduler. */
  def applyAndBroadcast(action: DiscussionAction): Task[DiscussionActionConfirmed] =
    for
      result <- discussionStore.applyAction(action)
      _ <- broadcastToAll(result)
      _ <- slackNotifier.notify(result, broadcastToAll)
    yield result

  private def handleTicketedAction(
    channel: OpenSpacesServerChannel,
    discussionAction: DiscussionAction,
  ): ZIO[Any, Throwable, Unit] =
    defer:
      val actionResult = discussionStore
        .applyAction(discussionAction)
        .run
      broadcastToAll(actionResult).run
      slackNotifier.notify(actionResult, broadcastToAll).run

  private def handleUnticketedAction(
    channel: OpenSpacesServerChannel,
    discussionAction: DiscussionAction,
  ): UIO[Unit] =
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
          ZIO.service[DiscussionStore].run,
          ZIO.service[AuthenticatedTicketService].run,
          ZIO.service[SlackNotifier].run,
        )
