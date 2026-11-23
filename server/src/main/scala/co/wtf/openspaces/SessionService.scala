package co.wtf.openspaces

import co.wtf.openspaces.auth.{AdminConfig, AuthenticatedTicketService}
import co.wtf.openspaces.db.{ConfirmedActionRepository, DiscussionRepository, UserRepository}
import co.wtf.openspaces.discussions.{DiscussionAction, DiscussionActionConfirmed, DiscussionState, DiscussionStore}
import co.wtf.openspaces.hackathon.*
import co.wtf.openspaces.lightning_talks.LightningTalkService
import co.wtf.openspaces.lighting_talks.*
import co.wtf.openspaces.activities.*
import co.wtf.openspaces.location.*
import co.wtf.openspaces.slack.SlackNotifier
import zio.*
import zio.direct.*
import zio.json.*
import WebSocketMessageFromClient.*
import WebSocketMessageFromServer.*

/** connected: authenticated channels, mapped to the user their ticket was issued for. */
private case class ChannelRegistry(
  connected: Map[OpenSpacesServerChannel, Person],
  pending: Map[OpenSpacesServerChannel, Vector[WebSocketMessageFromServer]],
)

/** Central session/connection manager for WebSocket clients.
  *
  * Responsibilities:
  * - WebSocket channel registry (connected clients, pending messages)
  * - Initial state sync on connect (discussions, lightning talks, hackathon projects)
  * - Broadcast hub for all confirmed actions to connected clients + Slack
  *
  * All entity-specific services (DiscussionStore, LightningTalkService, HackathonProjectService)
  * route their broadcasts through this service to ensure consistent delivery.
  */
case class SessionService(
  channelRegistry: Ref[ChannelRegistry],
  slackReplyCounts: Ref[SlackReplyCounts],
  discussionStore: DiscussionStore,
  lightningTalkService: LightningTalkService,
  hackathonProjectService: HackathonProjectService,
  activityService: ActivityService,
  locationService: LocationService,
  authenticatedTicketService: AuthenticatedTicketService,
  adminConfig: AdminConfig,
  slackNotifier: SlackNotifier,
  confirmedActionRepository: ConfirmedActionRepository,
  userRepository: UserRepository,
  discussionRepository: DiscussionRepository,
  channelToUser: Ref[Map[OpenSpacesServerChannel, Person]]):

  def handleMessage(
    message: WebSocketMessageFromClient,
    channel: OpenSpacesServerChannel,
  ): ZIO[Any, Throwable, Unit] =
    message match
      case TicketMessage(ticket) =>
        handleTicket(ticket, channel)
      case DiscussionActionMessage(action)       => handleEntityAction(discussions, channel, action)
      case LightningTalkActionMessage(action)    => handleEntityAction(lightningTalks, channel, action)
      case HackathonProjectActionMessage(action) => handleEntityAction(hackathonProjects, channel, action)
      case ActivityActionMessage(action)         => handleEntityAction(activities, channel, action)
      case LocationActionMessage(locationAction) =>
        // Only the connection's own user may share their location; ignore anything else.
        channelRegistry.get.flatMap { reg =>
          ZIO.when(reg.connected.get(channel).contains(locationAction.actor))(
            handleLocationAction(channel, locationAction),
          ).unit
        }
      case AuthorizationActionMessage(_) =>
        // Authorization changes go through the HTTP admin API, not the socket.
        ZIO.unit

  /** How SessionService routes one entity type's actions: authorize, apply, then
    * send rejections to the requester only and broadcast everything else (plus Slack).
    */
  private final case class EntityChannel[A <: ActorAction, C](
    wrap: C => WebSocketMessageFromServer,
    unauthorized: A => C,
    /** Rejected/Unauthorized results go only to the requesting channel. */
    isPrivate: C => Boolean,
    applyAction: A => Task[List[C]],
    notifySlack: (C, C => Task[Unit]) => Task[Unit],
    slackLabel: String,
  )

  private val discussions = EntityChannel[DiscussionAction, DiscussionActionConfirmed](
    wrap = DiscussionActionConfirmedMessage(_),
    unauthorized = DiscussionActionConfirmed.Unauthorized(_),
    isPrivate = {
      case _: DiscussionActionConfirmed.Rejected | _: DiscussionActionConfirmed.Unauthorized => true
      case _                                                                               => false
    },
    applyAction = discussionStore.applyAction(_).map(List(_)),
    notifySlack = slackNotifier.notifyDiscussion,
    slackLabel = "discussion",
  )

  private val lightningTalks = EntityChannel[LightningTalkAction, LightningTalkActionConfirmed](
    wrap = LightningTalkActionConfirmedMessage(_),
    unauthorized = LightningTalkActionConfirmed.Unauthorized(_),
    isPrivate = {
      case _: LightningTalkActionConfirmed.Rejected | _: LightningTalkActionConfirmed.Unauthorized => true
      case _                                                                                     => false
    },
    applyAction = lightningTalkService.applyAction(_).map(List(_)),
    notifySlack = slackNotifier.notifyLightning,
    slackLabel = "lightning",
  )

  private val hackathonProjects = EntityChannel[HackathonProjectAction, HackathonProjectActionConfirmed](
    wrap = HackathonProjectActionConfirmedMessage(_),
    unauthorized = HackathonProjectActionConfirmed.Unauthorized(_),
    isPrivate = {
      case _: HackathonProjectActionConfirmed.Rejected | _: HackathonProjectActionConfirmed.Unauthorized => true
      case _                                                                                           => false
    },
    applyAction = hackathonProjectService.applyAction,
    notifySlack = slackNotifier.notifyHackathonProject,
    slackLabel = "hackathon",
  )

  private val activities = EntityChannel[ActivityAction, ActivityActionConfirmed](
    wrap = ActivityActionConfirmedMessage(_),
    unauthorized = ActivityActionConfirmed.Unauthorized(_),
    isPrivate = {
      case _: ActivityActionConfirmed.Rejected | _: ActivityActionConfirmed.Unauthorized => true
      case _                                                                           => false
    },
    applyAction = activityService.applyAction(_).map(List(_)),
    notifySlack = slackNotifier.notifyActivity,
    slackLabel = "activity",
  )

  private def handleEntityAction[A <: ActorAction, C](
    entity: EntityChannel[A, C],
    channel: OpenSpacesServerChannel,
    action: A,
  ): Task[Unit] =
    isTicketedActionAuthorized(channel, action).flatMap { authorized =>
      if authorized then
        entity.applyAction(action).flatMap(ZIO.foreachDiscard(_)(publish(entity, _, Some(channel))))
      else
        channel.send(entity.wrap(entity.unauthorized(action))).ignore
    }

  /** Deliver a result: privately to the source channel, or to everyone plus Slack. */
  private def publish[A <: ActorAction, C](
    entity: EntityChannel[A, C],
    result: C,
    sourceChannel: Option[OpenSpacesServerChannel],
  ): Task[Unit] =
    if entity.isPrivate(result) then
      ZIO.foreachDiscard(sourceChannel)(_.send(entity.wrap(result)).ignore)
    else
      broadcastToAll(entity.wrap(result)) *>
        entity
          .notifySlack(result, followUp => broadcastToAll(entity.wrap(followUp)))
          .catchAll(err => ZIO.logError(s"Slack ${entity.slackLabel} notification failed (non-fatal): $err"))

  /** Generate a random action and broadcast to all connected clients */
  def randomDiscussionAction: Task[DiscussionActionConfirmed] =
    for
      action <- discussionStore.randomDiscussionAction
      _ <- publish(discussions, action, None)
    yield action

  /** Generate a random lightning participation action and broadcast. */
  def randomLightningAction: Task[LightningTalkActionConfirmed] =
    for
      action <- lightningTalkService.randomLightningAction
      _ <- publish(lightningTalks, action, None)
    yield action

  /** Generate random hackathon project actions and broadcast. */
  def randomHackathonAction: Task[List[HackathonProjectActionConfirmed]] =
    for
      actions <- hackathonProjectService.randomHackathonAction
      _ <- ZIO.foreachDiscard(actions)(publish(hackathonProjects, _, None))
    yield actions

  /** Generate random activity actions and broadcast. */
  def randomActivityAction: Task[ActivityActionConfirmed] =
    for
      action <- activityService.randomActivityAction
      _ <- publish(activities, action, None)
    yield action

  /** Generate a random schedule-only action (move/swap/unschedule) and broadcast */
  def randomScheduleAction: Task[DiscussionActionConfirmed] =
    for
      action <- discussionStore.randomScheduleAction
      _ <- publish(discussions, action, None)
    yield action

  /** Delete all topics using the standard delete logic (broadcasts to all clients).
    * Also truncates the confirmed_actions log.
    */
  def deleteAllTopics: Task[Int] =
    for
      state <- discussionStore.snapshot
      topicIds = state.data.keys.toList
      _ <- ZIO.foreachDiscard(topicIds) { topicId =>
        for
          result <- discussionStore.applyAction(DiscussionAction.Delete(topicId, UserOps.systemPerson))
          _ <- broadcastToAll(DiscussionActionConfirmedMessage(result))
        yield ()
      }
      _ <- confirmedActionRepository.truncate
    yield topicIds.size

  /** List all active discussion topics for admin tooling.
    * Uses a DB reload to avoid stale in-memory state in admin APIs.
    */
  def listAllTopics: Task[List[AdminTopicInfo]] =
    import neotype.unwrap
    discussionStore.reloadFromDatabase.map { state =>
      state.data.values.toList
        .sortBy(_.createdAtEpochMs)
        .map(d =>
          AdminTopicInfo(
            id = d.id.unwrap,
            topic = d.topic.unwrap,
            facilitator = d.facilitator.unwrap,
            votes = d.votes,
          )
        )
    }

  /** Delete one topic as an admin operation and force state reconciliation.
    *
    * This path bypasses the discussion action pipeline so admin cleanup is not
    * blocked by tertiary concerns (event-log/slack failures).
    */
  def deleteTopicAsAdmin(topicId: TopicId): Task[Boolean] =
    import neotype.unwrap
    for
      _ <- discussionRepository.softDelete(topicId.unwrap)
      reloadedDiscussionState <- discussionStore.reloadFromDatabase
      _ <- broadcastToAll(SessionService.discussionStateReplace(reloadedDiscussionState))
      _ <- slackNotifier
        .notifyDiscussion(
          DiscussionActionConfirmed.Delete(topicId),
          msg => broadcastToAll(DiscussionActionConfirmedMessage(msg)),
        )
        .catchAll(err => ZIO.logError(s"Admin delete Slack cleanup failed (non-fatal) for topic ${topicId.unwrap}: $err"))
        .ignore
    yield !reloadedDiscussionState.data.contains(topicId)

  /** Delete all records attributed to users in RandomUsers.pool.
    *
    * Strategy:
    * 1) Delete confirmed_actions rows for these actors (no FK to users)
    * 2) Delete users; FK cascades remove dependent rows
    * 3) Reload/broadcast all entity states to keep clients synchronized
    */
  def deleteAllRandomUserRecords: Task[DeleteRandomUsersResult] =
    import neotype.unwrap
    val usernames = RandomUsers.pool.map(_.unwrap).distinct
    for
      deletedConfirmedActions <- confirmedActionRepository.deleteByActors(usernames)
      deletedUsers <- userRepository.deleteByUsernames(usernames)
      _ <- reloadAllStateFromDatabase
    yield DeleteRandomUsersResult(
      deletedUsers = deletedUsers,
      deletedConfirmedActions = deletedConfirmedActions,
    )

  /** Reload all entity state from the database and broadcast full replacements.
    *
    * Useful when data was modified out-of-band (e.g. direct SQL edits) and
    * in-memory state needs to be reconciled without restarting the server.
    */
  def reloadAllStateFromDatabase: Task[ReloadStateResult] =
    for
      reloadedDiscussionState <- discussionStore.reloadFromDatabase
      reloadedLightningState <- lightningTalkService.reloadFromDatabase
      reloadedHackathonState <- hackathonProjectService.reloadFromDatabase
      reloadedActivityState <- activityService.reloadFromDatabase
      _ <- ZIO.foreachDiscard(
        SessionService.entityStateReplaces(
          reloadedDiscussionState,
          reloadedLightningState,
          reloadedHackathonState,
          reloadedActivityState,
        ),
      )(broadcastToAll)
    yield ReloadStateResult(
      discussions = reloadedDiscussionState.data.size,
      lightningTalks = reloadedLightningState.proposals.size,
      hackathonProjects = reloadedHackathonState.projects.size,
      activities = reloadedActivityState.activities.size,
    )

  private def handleTicket(
    ticket: Ticket,
    channel: OpenSpacesServerChannel,
  ): ZIO[Any, Throwable, Unit] =
    defer:
      val person = authenticatedTicketService
        .use(ticket)
        .mapError(new Exception(_))
        .run
      channelRegistry
        .update(reg =>
          reg.copy(pending = reg.pending.updated(channel, Vector.empty)),
        )
        .run
      val locationState = locationService.snapshot.run
      val initialMessages =
        SessionService.entityStateReplaces(
          discussionStore.snapshot.run,
          lightningTalkService.snapshot.run,
          hackathonProjectService.snapshot.run,
          activityService.snapshot.run,
        ) ++ Vector(
          LocationActionConfirmedMessage(
            LocationActionConfirmed.StateReplace(locationState.locations.values.toList),
          ),
          SlackReplyCountsMessage(slackReplyCounts.get.run),
        )
      val messagesToSend = channelRegistry
        .modify(reg =>
          val queued = reg.pending.getOrElse(channel, Vector.empty)
          (
            // Send the authoritative snapshots last so any queued stale deltas
            // (for example after out-of-band DB edits followed by a reload)
            // cannot override the fresh state on newly authenticated clients.
            queued ++ initialMessages,
            reg.copy(
              connected = reg.connected + (channel -> person),
              pending = reg.pending - channel,
            ),
          )
        )
        .run
      ZIO.foreachDiscard(messagesToSend)(message =>
        channel
          .send(message)
          .tapError(_ =>
            channelRegistry.update(reg =>
              reg.copy(
                connected = reg.connected - channel,
                pending = reg.pending - channel,
              ),
            ),
          )
          .ignore,
      ).run

  private def broadcastToAll(
    message: WebSocketMessageFromServer,
  ): Task[Unit] =
    defer:
      // Persist to confirmed_actions log (fire-and-forget, skip Rejected/Unauthorized/StateReplace)
      persistIfLoggable(message).ignore.run

      // Update server's in-memory state for outbound confirmed actions.
      //
      // TODO(double-apply): Every entity service already updates its own Ref inside
      // applyAction, so for normal actions this applies the same event a second time.
      // It is harmless today only because every state transition happens to be
      // idempotent (map upserts, set adds, filterNot). Pick a single writer:
      //   a) services own their state: only apply here for events that bypass the
      //      services (SlackThreadLinked follow-ups, admin StateReplace), or
      //   b) broadcast owns state: services stop calling state.update and this is
      //      the only place events are applied.
      // A non-idempotent transition (e.g. a counter) would silently double-count.
      message match
        case DiscussionActionConfirmedMessage(discussionConfirmed) =>
          discussionStore.applyConfirmed(discussionConfirmed).run
        case LightningTalkActionConfirmedMessage(lightningConfirmed) =>
          lightningTalkService.applyConfirmed(lightningConfirmed).run
        case HackathonProjectActionConfirmedMessage(hackathonConfirmed) =>
          hackathonProjectService.applyConfirmed(hackathonConfirmed).run
        case ActivityActionConfirmedMessage(activityConfirmed) =>
          activityService.applyConfirmed(activityConfirmed).run
        case LocationActionConfirmedMessage(locationConfirmed) =>
          locationService.applyConfirmed(locationConfirmed).run
        case SlackReplyCountsMessage(counts) =>
          slackReplyCounts.set(counts).run
        case _ =>
          ZIO.unit.run
      val registryBeforeBroadcast = channelRegistry.get.run
      message match
        case SlackReplyCountsMessage(_) =>
          ZIO.logInfo(
            s"Broadcasting SlackReplyCountsMessage to connected=${registryBeforeBroadcast.connected.size}, pending=${registryBeforeBroadcast.pending.size}"
          ).run
        case _ =>
          ZIO.unit.run
      val channels = channelRegistry
        .modify(reg =>
          val updatedPending =
            reg.pending.view.mapValues(buffer => buffer :+ message).toMap
          (
            reg.connected.keySet,
            reg.copy(pending = updatedPending),
          )
        )
        .run
      message match
        case _: SlackReplyCountsMessage =>
          ZIO
            .foreachParDiscard(channels)(channel =>
              channel
                .send(message)
                .tapError(err =>
                  ZIO.logError(s"Failed to broadcast SlackReplyCountsMessage: ${err.getMessage}")
                )
                .ignore,
            )
            .run
        case _ =>
          ZIO
            .foreachParDiscard(channels)(channel =>
              channel.send(message).ignore,
            )
            .run

  /** Persist state-changing actions to the confirmed_actions log. */
  private def persistIfLoggable(message: WebSocketMessageFromServer): Task[Unit] =
    import neotype.unwrap

    def persistAction[A <: HasActor & Product: zio.json.JsonEncoder](
      entityType: String,
      action: A,
    ): Task[Unit] =
      ZIO.when(action.isStateChange)(
        confirmedActionRepository.append(entityType, action.productPrefix, action.toJson, action.actor.map(_.unwrap)),
      ).unit

    message match
      case DiscussionActionConfirmedMessage(action)       => persistAction("Discussion", action)
      case LightningTalkActionConfirmedMessage(action)    => persistAction("LightningTalk", action)
      case HackathonProjectActionConfirmedMessage(action) => persistAction("HackathonProject", action)
      case ActivityActionConfirmedMessage(action)         => persistAction("Activity", action)
      case LocationActionConfirmedMessage(action)         => persistAction("Location", action)
      case _                                              => ZIO.unit

  /** Broadcast authorization granted to all connected clients */
  def broadcastAuthorizationGranted(username: String): Task[Unit] =
    broadcastToAll(AuthorizationActionConfirmedMessage(
      AuthorizationActionConfirmed.UserApproved(username)
    ))

  /** Broadcast authorization revoked to all connected clients */
  def broadcastAuthorizationRevoked(username: String): Task[Unit] =
    broadcastToAll(AuthorizationActionConfirmedMessage(
      AuthorizationActionConfirmed.UserRevoked(username)
    ))

  def removeChannel(channel: OpenSpacesServerChannel): Task[Unit] =
    for
      // Check if this channel had an associated user sharing location
      userOpt <- channelToUser.modify(m => (m.get(channel), m - channel))
      // If user was sharing, broadcast that they stopped
      _ <- userOpt match
        case Some(person) =>
          locationService.removeUser(person).flatMap {
            case Some(confirmed) => broadcastToAll(LocationActionConfirmedMessage(confirmed))
            case None => ZIO.unit
          }
        case None => ZIO.unit
      _ <- channelRegistry.update(reg =>
        reg.copy(
          connected = reg.connected - channel,
          pending = reg.pending - channel,
        ),
      )
    yield ()

  /** Apply an action and broadcast to all clients + Slack. Used by scheduler. */
  def applyAndBroadcast(action: DiscussionAction): Task[DiscussionActionConfirmed] =
    for
      result <- discussionStore.applyAction(action)
      _ <- publish(discussions, result, None)
    yield result

  private def handleLocationAction(
    channel: OpenSpacesServerChannel,
    locationAction: LocationAction,
  ): Task[Unit] =
    for
      // Track which user is on which channel for disconnect cleanup
      _ <- locationAction match
        case LocationAction.StartSharing(person, _, _, _) =>
          channelToUser.update(_ + (channel -> person))
        case LocationAction.StopSharing(person) =>
          channelToUser.update(_ - channel)
        case _ => ZIO.unit
      result <- locationService.applyAction(locationAction)
      _ <- broadcastToAll(LocationActionConfirmedMessage(result))
    yield ()

  /** Start background fiber to expire stale locations (every 10 minutes) */
  def startLocationExpirationCheck(interval: Duration = 10.minutes): Task[Fiber.Runtime[Throwable, Unit]] =
    val checkLoop =
      (for
        expired <- locationService.expireStaleLocations
        _ <- ZIO.foreachDiscard(expired)(confirmed =>
          broadcastToAll(LocationActionConfirmedMessage(confirmed))
        )
      yield ()).catchAll { error =>
        ZIO.logError(s"Error checking location expiration: ${error.getMessage}")
      }

    checkLoop.repeat(Schedule.fixed(interval)).unit.fork

  /** An action is authorized when:
    * - the channel authenticated with a ticket,
    * - the action's actor is the user that ticket was issued for (no acting on behalf of others),
    * - that user is approved (or an admin), and
    * - admin-only actions come from an admin.
    *
    * Ownership rules (e.g. only a facilitator may rename their topic) live in the
    * entity services, which know the current owners.
    */
  private def isTicketedActionAuthorized(
    channel: OpenSpacesServerChannel,
    action: ActorAction,
  ): Task[Boolean] =
    import neotype.unwrap
    channelRegistry.get.map(_.connected.get(channel)).flatMap {
      case Some(user) if user == action.actor =>
        val username = user.unwrap
        if adminConfig.isAdmin(username) then ZIO.succeed(true)
        else if requiresAdmin(action) then ZIO.succeed(false)
        else userRepository.isApproved(username)
      case _ =>
        ZIO.succeed(false)
    }

  private def requiresAdmin(action: ActorAction): Boolean =
    action match
      case _: DiscussionAction.Delete            => true
      case _: DiscussionAction.SetLockedTimeslot => true
      case _: LightningTalkAction.SetAssignment  => true
      case _: LightningTalkAction.DrawForNextNight => true
      case _                                     => false

  /** Start the background Slack reply count refresh.
    * Periodically fetches reply counts for all threads and broadcasts to clients.
    */
  def startSlackReplyCountRefresh(interval: Duration = 3.minutes): Task[Fiber.Runtime[Throwable, Unit]] =
    val refreshLoop =
      (for
        startedAt <- Clock.nanoTime
        _ <- ZIO.logInfo("Fetching Slack reply counts...")
        counts <- slackNotifier.fetchReplyCounts
        _ <- slackReplyCounts.set(counts)
        total =
          counts.discussions.size +
            counts.lightningTalks.size +
            counts.hackathonProjects.size +
            counts.activities.size
        totalReplies =
          counts.discussions.values.sum +
            counts.lightningTalks.values.sum +
            counts.hackathonProjects.values.sum +
            counts.activities.values.sum
        _ <- ZIO.when(total > 0)(
          ZIO.logInfo(s"Broadcasting reply counts for $total threads (total replies: $totalReplies)") *>
            broadcastToAll(SlackReplyCountsMessage(counts))
        )
        finishedAt <- Clock.nanoTime
        elapsedSeconds = (finishedAt - startedAt) / 1000000000L
        _ <- ZIO.logInfo(s"Slack reply count refresh cycle completed in ${elapsedSeconds}s")
      yield ()).catchAll { error =>
        ZIO.logError(s"Error fetching Slack reply counts: ${error.getMessage}")
      }

    refreshLoop.repeat(Schedule.fixed(interval)).unit.fork

  /** Start a lightweight websocket heartbeat so Heroku does not close idle sockets. */
  def startWebSocketKeepAlive(interval: Duration = 25.seconds): Task[Fiber.Runtime[Throwable, Unit]] =
    (ZIO.sleep(interval) *>
      channelRegistry.get.flatMap(reg =>
        ZIO.foreachParDiscard(reg.connected.keys)(channel =>
          channel.send(KeepAliveMessage).ignore,
        ),
      )).forever
      .fork

object SessionService:
  def discussionStateReplace(state: DiscussionState): WebSocketMessageFromServer =
    DiscussionActionConfirmedMessage(
      DiscussionActionConfirmed.StateReplace(state.data.values.toList, state.slots),
    )

  /** Full-state replacements for every persisted entity type. */
  def entityStateReplaces(
    discussions: DiscussionState,
    lightningTalks: LightningTalkState,
    hackathonProjects: HackathonProjectState,
    activities: ActivityState,
  ): Vector[WebSocketMessageFromServer] =
    Vector(
      discussionStateReplace(discussions),
      LightningTalkActionConfirmedMessage(
        LightningTalkActionConfirmed.StateReplace(lightningTalks.proposals.values.toList),
      ),
      HackathonProjectActionConfirmedMessage(
        HackathonProjectActionConfirmed.StateReplace(hackathonProjects.projects.values.toList),
      ),
      ActivityActionConfirmedMessage(
        ActivityActionConfirmed.StateReplace(activities.activities.values.toList),
      ),
    )

  val layer =
    ZLayer.fromZIO:
      defer:
        SessionService(
          Ref.make(
            ChannelRegistry(
              connected = Map.empty,
              pending = Map.empty,
            ),
          ).run,
          Ref.make(
            SlackReplyCounts(
              discussions = Map.empty,
              lightningTalks = Map.empty,
              hackathonProjects = Map.empty,
              activities = Map.empty,
            ),
          ).run,
          ZIO.service[DiscussionStore].run,
          ZIO.service[LightningTalkService].run,
          ZIO.service[HackathonProjectService].run,
          ZIO.service[ActivityService].run,
          ZIO.service[LocationService].run,
          ZIO.service[AuthenticatedTicketService].run,
          ZIO.service[AdminConfig].run,
          ZIO.service[SlackNotifier].run,
          ZIO.service[ConfirmedActionRepository].run,
          ZIO.service[UserRepository].run,
          ZIO.service[DiscussionRepository].run,
          Ref.make(Map.empty[OpenSpacesServerChannel, Person]).run,
        )
