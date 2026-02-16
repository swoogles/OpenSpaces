package co.wtf.openspaces

import co.wtf.openspaces.slack.SlackNotifier
import zio.*
import zio.direct.*
import zio.http.*
import zio.json.*

private case class ChannelRegistry(
  connected: Set[OpenSpacesServerChannel],
  pending: Map[OpenSpacesServerChannel, Vector[WebSocketMessage]],
)

case class DiscussionService(
  channelRegistry: Ref[ChannelRegistry],
  discussionStore: DiscussionStore,
  lightningTalkService: LightningTalkService,
  authenticatedTicketService: AuthenticatedTicketService,
  slackNotifier: SlackNotifier):

  def handleMessage(
    message: WebSocketMessage,
    channel: OpenSpacesServerChannel,
  ): ZIO[Any, Throwable, Unit] =
    message match
      // TODO Decide what should happen when *Confirmed messages come through here. Just no op? Error?
      case ticket: Ticket =>
        handleTicket(ticket, channel)
      case DiscussionActionMessage(discussionAction) =>
        defer:
          if (channelRegistry.get.run.connected.contains(channel)) {
            handleTicketedAction(channel, discussionAction).run
          }
          else {
            handleUnticketedAction(channel, discussionAction).run
          }
      case LightningTalkActionMessage(lightningAction) =>
        defer:
          if (channelRegistry.get.run.connected.contains(channel)) {
            handleTicketedLightningAction(channel, lightningAction).run
          }
          else {
            handleUnticketedLightningAction(channel, lightningAction).run
          }

  /** Generate a random action and broadcast to all connected clients */
  def randomDiscussionAction: Task[DiscussionActionConfirmed] =
    for
      action <- discussionStore.randomDiscussionAction
      _ <- handleActionResult(action, None)
    yield action

  /** Generate a random lightning participation action and broadcast. */
  def randomLightningAction: Task[LightningTalkActionConfirmed] =
    for
      action <- lightningTalkService.randomLightningAction
      _ <- handleLightningActionResult(action, None)
    yield action

  /** Generate a random schedule-only action (move/swap/unschedule) and broadcast */
  def randomScheduleAction: Task[DiscussionActionConfirmed] =
    for
      action <- discussionStore.randomScheduleAction
      _ <- handleActionResult(action, None)
    yield action

  /** Delete all topics using the standard delete logic (broadcasts to all clients) */
  def deleteAllTopics: Task[Int] =
    for
      state <- discussionStore.snapshot
      topicIds = state.data.keys.toList
      _ <- ZIO.foreachDiscard(topicIds) { topicId =>
        for
          result <- discussionStore.applyAction(DiscussionAction.Delete(topicId))
          _ <- broadcastToAll(DiscussionActionConfirmedMessage(result))
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
      channelRegistry
        .update(reg =>
          reg.copy(pending = reg.pending.updated(channel, Vector.empty)),
        )
        .run
      val state = discussionStore.snapshot.run
      val lightningState = lightningTalkService.snapshot.run
      channel
        .send(
          DiscussionActionConfirmedMessage(
            DiscussionActionConfirmed.StateReplace(
              state.data.values.toList,
            ),
          ),
        )
        .tapError(_ =>
          channelRegistry.update(reg =>
            reg.copy(
              connected = reg.connected - channel,
              pending = reg.pending - channel,
            ),
          ),
        )
        .run
      channel
        .send(
          LightningTalkActionConfirmedMessage(
            LightningTalkActionConfirmed.StateReplace(
              lightningState.proposals.values.toList,
            ),
          ),
        )
        .tapError(_ =>
          channelRegistry.update(reg =>
            reg.copy(
              connected = reg.connected - channel,
              pending = reg.pending - channel,
            ),
          ),
        )
        .run
      val buffered = channelRegistry
        .modify(reg =>
          val queued = reg.pending.getOrElse(channel, Vector.empty)
          (
            queued,
            reg.copy(
              connected = reg.connected + channel,
              pending = reg.pending - channel,
            ),
          )
        )
        .run
      ZIO.foreachDiscard(buffered)(message => channel.send(message).ignore).run

  private def broadcastToAll(message: WebSocketMessage): Task[Unit] =
    defer:
      // Update server's in-memory state for outbound confirmed actions.
      message match
        case DiscussionActionConfirmedMessage(discussionConfirmed) =>
          discussionStore.applyConfirmed(discussionConfirmed).run
        case LightningTalkActionConfirmedMessage(lightningConfirmed) =>
          lightningTalkService.applyConfirmed(lightningConfirmed).run
        case _ =>
          ZIO.unit.run
      val channels = channelRegistry
        .modify(reg =>
          val updatedPending =
            reg.pending.view.mapValues(buffer => buffer :+ message).toMap
          (
            reg.connected,
            reg.copy(pending = updatedPending),
          )
        )
        .run
      ZIO
        .foreachParDiscard(channels)(channel =>
          channel.send(message).ignore,
        )
        .run

  def removeChannel(channel: OpenSpacesServerChannel): UIO[Unit] =
    channelRegistry
      .update(reg =>
        reg.copy(
          connected = reg.connected - channel,
          pending = reg.pending - channel,
        ),
      )
      .unit

  /** Apply an action and broadcast to all clients + Slack. Used by scheduler. */
  def applyAndBroadcast(action: DiscussionAction): Task[DiscussionActionConfirmed] =
    for
      result <- discussionStore.applyAction(action)
      _ <- handleActionResult(result, None)
    yield result

  private def handleTicketedAction(
    channel: OpenSpacesServerChannel,
    discussionAction: DiscussionAction,
  ): ZIO[Any, Throwable, Unit] =
    defer:
      val actionResult = discussionStore
        .applyAction(discussionAction)
        .run
      handleActionResult(actionResult, Some(channel)).run

  private def handleActionResult(
    actionResult: DiscussionActionConfirmed,
    sourceChannel: Option[OpenSpacesServerChannel],
  ): Task[Unit] =
    actionResult match
      case rejected @ DiscussionActionConfirmed.Rejected(_) =>
        sourceChannel match
          case Some(channel) => channel.send(DiscussionActionConfirmedMessage(rejected)).ignore
          case None => ZIO.unit
      case other =>
        broadcastToAll(DiscussionActionConfirmedMessage(other)) *> slackNotifier.notifyDiscussion(
          other,
          msg => broadcastToAll(DiscussionActionConfirmedMessage(msg)),
        )

  private def handleLightningActionResult(
    actionResult: LightningTalkActionConfirmed,
    sourceChannel: Option[OpenSpacesServerChannel],
  ): Task[Unit] =
    actionResult match
      case rejected @ LightningTalkActionConfirmed.Rejected(_) =>
        sourceChannel match
          case Some(channel) => channel.send(LightningTalkActionConfirmedMessage(rejected)).ignore
          case None => ZIO.unit
      case other =>
        broadcastToAll(LightningTalkActionConfirmedMessage(other)) *> slackNotifier.notifyLightning(
          other,
          msg => broadcastToAll(LightningTalkActionConfirmedMessage(msg)),
        )

  private def handleUnticketedAction(
    channel: OpenSpacesServerChannel,
    discussionAction: DiscussionAction,
  ): UIO[Unit] =
    channel
      .send(
        DiscussionActionConfirmedMessage(
          DiscussionActionConfirmed.Unauthorized(
            discussionAction,
          ),
        ),
      )
      .ignore

  private def handleTicketedLightningAction(
    channel: OpenSpacesServerChannel,
    lightningAction: LightningTalkAction,
  ): ZIO[Any, Throwable, Unit] =
    defer:
      val actionResult = lightningTalkService
        .applyAction(lightningAction)
        .run
      handleLightningActionResult(actionResult, Some(channel)).run

  private def handleUnticketedLightningAction(
    channel: OpenSpacesServerChannel,
    lightningAction: LightningTalkAction,
  ): UIO[Unit] =
    channel
      .send(
        LightningTalkActionConfirmedMessage(
          LightningTalkActionConfirmed.Unauthorized(
            lightningAction,
          ),
        ),
      )
      .ignore

object DiscussionService:
  val layer =
    ZLayer.fromZIO:
      defer:
        DiscussionService(
          Ref.make(
            ChannelRegistry(
              connected = Set.empty,
              pending = Map.empty,
            ),
          ).run,
          ZIO.service[DiscussionStore].run,
          ZIO.service[LightningTalkService].run,
          ZIO.service[AuthenticatedTicketService].run,
          ZIO.service[SlackNotifier].run,
        )
