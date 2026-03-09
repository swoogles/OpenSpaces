package co.wtf.openspaces

import co.wtf.openspaces.auth.{AdminConfig, AuthenticatedTicketService}
import co.wtf.openspaces.db.{ConfirmedActionRepository, DiscussionRepository, UserRepository}
import co.wtf.openspaces.discussions.{DiscussionAction, DiscussionActionConfirmed, DiscussionState, DiscussionStore}
import co.wtf.openspaces.hackathon.*
import co.wtf.openspaces.lightning_talks.LightningTalkService
import co.wtf.openspaces.lighting_talks.*
import co.wtf.openspaces.activities.*
import co.wtf.openspaces.activities.ActivityService
import co.wtf.openspaces.location.*
import co.wtf.openspaces.slack.SlackNotifier
import zio.*
import zio.direct.*
import zio.http.*
import zio.json.*
import WebSocketMessageFromClient.*
import WebSocketMessageFromServer.*

private case class ChannelRegistry(
  connected: Set[OpenSpacesServerChannel],
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
      case HackathonProjectActionMessage(hackathonAction) =>
        defer:
          if (channelRegistry.get.run.connected.contains(channel)) {
            handleTicketedHackathonAction(channel, hackathonAction).run
          }
          else {
            handleUnticketedHackathonAction(channel, hackathonAction).run
          }
      case ActivityActionMessage(activityAction) =>
        defer:
          if (channelRegistry.get.run.connected.contains(channel)) {
            handleTicketedActivityAction(channel, activityAction).run
          }
          else {
            handleUnticketedActivityAction(channel, activityAction).run
          }
      case LocationActionMessage(locationAction) =>
        defer:
          if (channelRegistry.get.run.connected.contains(channel)) {
            handleLocationAction(channel, locationAction).run
          }
          else {
            ZIO.unit.run // Ignore location actions from unauthenticated channels
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

  /** Generate random hackathon project actions and broadcast. */
  def randomHackathonAction: Task[List[HackathonProjectActionConfirmed]] =
    for
      actions <- hackathonProjectService.randomHackathonAction
      _ <- ZIO.foreachDiscard(actions)(action => handleHackathonActionResult(action, None))
    yield actions

  /** Generate random activity actions and broadcast. */
  def randomActivityAction: Task[ActivityActionConfirmed] =
    for
      action <- activityService.randomActivityAction
      _ <- handleActivityActionResult(action, None)
    yield action

  /** Generate a random schedule-only action (move/swap/unschedule) and broadcast */
  def randomScheduleAction: Task[DiscussionActionConfirmed] =
    for
      action <- discussionStore.randomScheduleAction
      _ <- handleActionResult(action, None)
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
          result <- discussionStore.applyAction(DiscussionAction.Delete(topicId))
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
      _ <- broadcastToAll(
        DiscussionActionConfirmedMessage(
          DiscussionActionConfirmed.StateReplace(
            reloadedDiscussionState.data.values.toList,
            reloadedDiscussionState.slots,
          ),
        ),
      )
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
      reloadedDiscussionState <- discussionStore.reloadFromDatabase
      reloadedLightningState <- lightningTalkService.reloadFromDatabase
      reloadedHackathonState <- hackathonProjectService.reloadFromDatabase
      reloadedActivityState <- activityService.reloadFromDatabase
      _ <- broadcastToAll(
        DiscussionActionConfirmedMessage(
          DiscussionActionConfirmed.StateReplace(reloadedDiscussionState.data.values.toList, reloadedDiscussionState.slots),
        ),
      )
      _ <- broadcastToAll(
        LightningTalkActionConfirmedMessage(
          LightningTalkActionConfirmed.StateReplace(reloadedLightningState.proposals.values.toList),
        ),
      )
      _ <- broadcastToAll(
        HackathonProjectActionConfirmedMessage(
          HackathonProjectActionConfirmed.StateReplace(reloadedHackathonState.projects.values.toList),
        ),
      )
      _ <- broadcastToAll(
        ActivityActionConfirmedMessage(
          ActivityActionConfirmed.StateReplace(reloadedActivityState.activities.values.toList),
        ),
      )
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
      _ <- broadcastToAll(
        DiscussionActionConfirmedMessage(
          DiscussionActionConfirmed.StateReplace(
            reloadedDiscussionState.data.values.toList,
            reloadedDiscussionState.slots,
          ),
        ),
      )
      _ <- broadcastToAll(
        LightningTalkActionConfirmedMessage(
          LightningTalkActionConfirmed.StateReplace(
            reloadedLightningState.proposals.values.toList,
          ),
        ),
      )
      _ <- broadcastToAll(
        HackathonProjectActionConfirmedMessage(
          HackathonProjectActionConfirmed.StateReplace(
            reloadedHackathonState.projects.values.toList,
          ),
        ),
      )
      _ <- broadcastToAll(
        ActivityActionConfirmedMessage(
          ActivityActionConfirmed.StateReplace(
            reloadedActivityState.activities.values.toList,
          ),
        ),
      )
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
      val hackathonState = hackathonProjectService.snapshot.run
      val activityState = activityService.snapshot.run
      val locationState = locationService.snapshot.run
      val latestSlackReplyCounts = slackReplyCounts.get.run
      val initialMessages = Vector[WebSocketMessageFromServer](
        DiscussionActionConfirmedMessage(
          DiscussionActionConfirmed.StateReplace(
            state.data.values.toList,
            state.slots,
          ),
        ),
        LightningTalkActionConfirmedMessage(
          LightningTalkActionConfirmed.StateReplace(
            lightningState.proposals.values.toList,
          ),
        ),
        HackathonProjectActionConfirmedMessage(
          HackathonProjectActionConfirmed.StateReplace(
            hackathonState.projects.values.toList,
          ),
        ),
        ActivityActionConfirmedMessage(
          ActivityActionConfirmed.StateReplace(
            activityState.activities.values.toList,
          ),
        ),
        LocationActionConfirmedMessage(
          LocationActionConfirmed.StateReplace(
            locationState.locations.values.toList,
          ),
        ),
        SlackReplyCountsMessage(latestSlackReplyCounts),
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
              connected = reg.connected + channel,
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
            reg.connected,
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

  /** Persist action to confirmed_actions log, skipping Rejected/Unauthorized/StateReplace. */
  private def persistIfLoggable(message: WebSocketMessageFromServer): Task[Unit] =
    import neotype.unwrap

    def isLoggable(actionName: String): Boolean =
      !actionName.contains("Rejected") &&
      !actionName.contains("Unauthorized") &&
      !actionName.contains("StateReplace")

    def persistAction[A <: HasActor: zio.json.JsonEncoder](
      entityType: String,
      action: A,
    ): Task[Unit] =
      val actionType = action.getClass.getSimpleName.stripSuffix("$")
      if isLoggable(actionType) then
        confirmedActionRepository
          .append(entityType, actionType, action.toJson, action.actor.map(_.unwrap))
          .unit
      else
        ZIO.unit

    message match
      case DiscussionActionConfirmedMessage(action) => persistAction("Discussion", action)
      case LightningTalkActionConfirmedMessage(action) => persistAction("LightningTalk", action)
      case HackathonProjectActionConfirmedMessage(action) => persistAction("HackathonProject", action)
      case ActivityActionConfirmedMessage(action) => persistAction("Activity", action)
      case LocationActionConfirmedMessage(action) => persistAction("Location", action)
      case _ => ZIO.unit

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
      _ <- handleActionResult(result, None)
    yield result

  private def handleTicketedAction(
    channel: OpenSpacesServerChannel,
    discussionAction: DiscussionAction,
  ): ZIO[Any, Throwable, Unit] =
    defer:
      val authorized = isTicketedActionAuthorized(channel, extractDiscussionActorFromAction(discussionAction)).run
      if !authorized then
        channel
          .send(
            DiscussionActionConfirmedMessage(
              DiscussionActionConfirmed.Unauthorized(discussionAction),
            ),
          )
          .ignore
          .run
      else
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
      case unauthorized @ DiscussionActionConfirmed.Unauthorized(_) =>
        sourceChannel match
          case Some(channel) => channel.send(DiscussionActionConfirmedMessage(unauthorized)).ignore
          case None => ZIO.unit
      case other =>
        broadcastToAll(DiscussionActionConfirmedMessage(other)) *>
          slackNotifier
            .notifyDiscussion(
              other,
              msg => broadcastToAll(DiscussionActionConfirmedMessage(msg)),
            )
            .catchAll(err => ZIO.logError(s"Slack discussion notification failed (non-fatal): $err"))
            .ignore

  private def handleLightningActionResult(
    actionResult: LightningTalkActionConfirmed,
    sourceChannel: Option[OpenSpacesServerChannel],
  ): Task[Unit] =
    actionResult match
      case rejected @ LightningTalkActionConfirmed.Rejected(_) =>
        sourceChannel match
          case Some(channel) => channel.send(LightningTalkActionConfirmedMessage(rejected)).ignore
          case None => ZIO.unit
      case unauthorized @ LightningTalkActionConfirmed.Unauthorized(_) =>
        sourceChannel match
          case Some(channel) => channel.send(LightningTalkActionConfirmedMessage(unauthorized)).ignore
          case None => ZIO.unit
      case other =>
        broadcastToAll(LightningTalkActionConfirmedMessage(other)) *>
          slackNotifier
            .notifyLightning(
              other,
              msg => broadcastToAll(LightningTalkActionConfirmedMessage(msg)),
            )
            .catchAll(err => ZIO.logError(s"Slack lightning notification failed (non-fatal): $err"))
            .ignore

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
      val authorized = isTicketedActionAuthorized(channel, extractLightningActorFromAction(lightningAction)).run
      if !authorized then
        channel
          .send(
            LightningTalkActionConfirmedMessage(
              LightningTalkActionConfirmed.Unauthorized(lightningAction),
            ),
          )
          .ignore
          .run
      else
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

  private def handleTicketedHackathonAction(
    channel: OpenSpacesServerChannel,
    hackathonAction: HackathonProjectAction,
  ): ZIO[Any, Throwable, Unit] =
    defer:
      val authorized = isTicketedActionAuthorized(channel, extractHackathonActorFromAction(hackathonAction)).run
      if !authorized then
        channel
          .send(
            HackathonProjectActionConfirmedMessage(
              HackathonProjectActionConfirmed.Unauthorized(hackathonAction),
            ),
          )
          .ignore
          .run
      else
        val actionResults = hackathonProjectService
          .applyAction(hackathonAction)
          .run
        ZIO.foreachDiscard(actionResults)(result =>
          handleHackathonActionResult(result, Some(channel))
        ).run

  private def handleHackathonActionResult(
    actionResult: HackathonProjectActionConfirmed,
    sourceChannel: Option[OpenSpacesServerChannel],
  ): Task[Unit] =
    actionResult match
      case rejected @ HackathonProjectActionConfirmed.Rejected(_) =>
        sourceChannel match
          case Some(channel) => channel.send(HackathonProjectActionConfirmedMessage(rejected)).ignore
          case None => ZIO.unit
      case unauthorized @ HackathonProjectActionConfirmed.Unauthorized(_) =>
        sourceChannel match
          case Some(channel) => channel.send(HackathonProjectActionConfirmedMessage(unauthorized)).ignore
          case None => ZIO.unit
      case other =>
        broadcastToAll(HackathonProjectActionConfirmedMessage(other)) *>
          slackNotifier
            .notifyHackathonProject(
              other,
              msg => broadcastToAll(HackathonProjectActionConfirmedMessage(msg)),
            )
            .catchAll(err => ZIO.logError(s"Slack hackathon notification failed (non-fatal): $err"))
            .ignore

  private def handleUnticketedHackathonAction(
    channel: OpenSpacesServerChannel,
    hackathonAction: HackathonProjectAction,
  ): UIO[Unit] =
    channel
      .send(
        HackathonProjectActionConfirmedMessage(
          HackathonProjectActionConfirmed.Unauthorized(
            hackathonAction,
          ),
        ),
      )
      .ignore

  private def handleTicketedActivityAction(
    channel: OpenSpacesServerChannel,
    activityAction: ActivityAction,
  ): ZIO[Any, Throwable, Unit] =
    defer:
      val authorized = isTicketedActionAuthorized(channel, extractActivityActorFromAction(activityAction)).run
      if !authorized then
        channel
          .send(
            ActivityActionConfirmedMessage(
              ActivityActionConfirmed.Unauthorized(activityAction),
            ),
          )
          .ignore
          .run
      else
        val actionResult = activityService
          .applyAction(activityAction)
          .run
        handleActivityActionResult(actionResult, Some(channel)).run

  private def handleActivityActionResult(
    actionResult: ActivityActionConfirmed,
    sourceChannel: Option[OpenSpacesServerChannel],
  ): Task[Unit] =
    actionResult match
      case rejected @ ActivityActionConfirmed.Rejected(_) =>
        sourceChannel match
          case Some(channel) => channel.send(ActivityActionConfirmedMessage(rejected)).ignore
          case None => ZIO.unit
      case unauthorized @ ActivityActionConfirmed.Unauthorized(_) =>
        sourceChannel match
          case Some(channel) => channel.send(ActivityActionConfirmedMessage(unauthorized)).ignore
          case None => ZIO.unit
      case other =>
        broadcastToAll(ActivityActionConfirmedMessage(other)) *>
          slackNotifier
            .notifyActivity(
              other,
              msg => broadcastToAll(ActivityActionConfirmedMessage(msg)),
            )
            .catchAll(err => ZIO.logError(s"Slack activity notification failed (non-fatal): $err"))
            .ignore

  private def handleUnticketedActivityAction(
    channel: OpenSpacesServerChannel,
    activityAction: ActivityAction,
  ): UIO[Unit] =
    channel
      .send(
        ActivityActionConfirmedMessage(
          ActivityActionConfirmed.Unauthorized(
            activityAction,
          ),
        ),
      )
      .ignore

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

    (checkLoop *> checkLoop.schedule(Schedule.fixed(interval)).unit).fork

  private def isTicketedActionAuthorized(
    channel: OpenSpacesServerChannel,
    actionActor: Option[Person],
  ): Task[Boolean] =
    defer:
      val isConnected = channelRegistry.get.run.connected.contains(channel)
      if !isConnected then false
      else
        actionActor match
          case Some(actor) =>
            import neotype.unwrap
            val username = actor.unwrap
            if adminConfig.isAdmin(username) then true
            else userRepository.isApproved(username).run
          case None =>
            true

  private def extractDiscussionActorFromAction(action: DiscussionAction): Option[Person] =
    action match
      case DiscussionAction.Add(_, facilitator) => Some(facilitator)
      case DiscussionAction.AddWithRoomSlot(_, facilitator, _) => Some(facilitator)
      case DiscussionAction.Vote(_, feedback) => Some(feedback.voter)
      case DiscussionAction.ResetUser(person) => Some(person)
      case _ => None

  private def extractLightningActorFromAction(action: LightningTalkAction): Option[Person] =
    action match
      case LightningTalkAction.SetParticipation(speaker, _) => Some(speaker)
      case _ => None

  private def extractHackathonActorFromAction(action: HackathonProjectAction): Option[Person] =
    action match
      case HackathonProjectAction.Create(_, creator) => Some(creator)
      case HackathonProjectAction.Join(_, person) => Some(person)
      case HackathonProjectAction.Leave(_, person) => Some(person)
      case HackathonProjectAction.Delete(_, requester) => Some(requester)
      case _ => None

  private def extractActivityActorFromAction(action: ActivityAction): Option[Person] =
    action match
      case ActivityAction.Create(_, _, creator) => Some(creator)
      case ActivityAction.SetInterest(_, person, _) => Some(person)
      case ActivityAction.Update(_, _, _, editor) => Some(editor)
      case ActivityAction.Delete(_, requester) => Some(requester)

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

    (refreshLoop *> refreshLoop.schedule(Schedule.fixed(interval)).unit).fork

  /** Start a lightweight websocket heartbeat so Heroku does not close idle sockets. */
  def startWebSocketKeepAlive(interval: Duration = 25.seconds): Task[Fiber.Runtime[Throwable, Unit]] =
    (ZIO.sleep(interval) *>
      channelRegistry.get.flatMap(reg =>
        ZIO.foreachParDiscard(reg.connected)(channel =>
          channel.send(KeepAliveMessage).ignore,
        ),
      )).forever
      .fork

object SessionService:
  val layer =
    ZLayer.fromZIO:
      defer:
        SessionService(
          Ref.make(
            ChannelRegistry(
              connected = Set.empty,
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
