package co.wtf.openspaces

import co.wtf.openspaces.auth.{AdminConfig, AuthenticatedTicketService}
import co.wtf.openspaces.db.{ConfirmedActionRepository, UserRepository}
import co.wtf.openspaces.discussions.{DiscussionAction, DiscussionActionConfirmed, DiscussionState, DiscussionStore}
import co.wtf.openspaces.hackathon.*
import co.wtf.openspaces.lightning_talks.LightningTalkService
import co.wtf.openspaces.lighting_talks.*
import co.wtf.openspaces.activities.*
import co.wtf.openspaces.activities.ActivityService
import co.wtf.openspaces.slack.SlackNotifier
import zio.*
import zio.direct.*
import zio.http.*
import zio.json.*

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
  discussionStore: DiscussionStore,
  lightningTalkService: LightningTalkService,
  hackathonProjectService: HackathonProjectService,
  activityService: ActivityService,
  authenticatedTicketService: AuthenticatedTicketService,
  adminConfig: AdminConfig,
  slackNotifier: SlackNotifier,
  confirmedActionRepository: ConfirmedActionRepository,
  userRepository: UserRepository):

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

  /** Delete one topic as an admin operation (bypasses ownership checks). */
  def deleteTopicAsAdmin(topicId: TopicId): Task[DiscussionActionConfirmed] =
    for
      result <- discussionStore.applyAction(DiscussionAction.Delete(topicId))
      _ <- handleActionResult(result, None)
      _ <- result match
        case DiscussionActionConfirmed.Delete(_) =>
          for
            reloadedDiscussionState <- discussionStore.reloadFromDatabase
            _ <- broadcastToAll(
              DiscussionActionConfirmedMessage(
                DiscussionActionConfirmed.StateReplace(
                  reloadedDiscussionState.data.values.toList,
                  reloadedDiscussionState.slots,
                ),
              ),
            )
          yield ()
        case _ =>
          ZIO.unit
    yield result

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
      channel
        .send(
          DiscussionActionConfirmedMessage(
            DiscussionActionConfirmed.StateReplace(
              state.data.values.toList,
              state.slots,
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
      channel
        .send(
          HackathonProjectActionConfirmedMessage(
            HackathonProjectActionConfirmed.StateReplace(
              hackathonState.projects.values.toList,
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
          ActivityActionConfirmedMessage(
            ActivityActionConfirmed.StateReplace(
              activityState.activities.values.toList,
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

  /** Persist action to confirmed_actions log, skipping Rejected/Unauthorized/StateReplace. */
  private def persistIfLoggable(message: WebSocketMessageFromServer): Task[Unit] =
    message match
      case DiscussionActionConfirmedMessage(action) =>
        action match
          case _: DiscussionActionConfirmed.Rejected     => ZIO.unit
          case _: DiscussionActionConfirmed.Unauthorized => ZIO.unit
          case _: DiscussionActionConfirmed.StateReplace => ZIO.unit
          case _ =>
            val actionType = action.getClass.getSimpleName.stripSuffix("$")
            val actor = extractDiscussionActor(action)
            confirmedActionRepository
              .append("Discussion", actionType, action.toJson, actor)
              .unit
      case LightningTalkActionConfirmedMessage(action) =>
        action match
          case _: LightningTalkActionConfirmed.Rejected     => ZIO.unit
          case _: LightningTalkActionConfirmed.Unauthorized => ZIO.unit
          case _: LightningTalkActionConfirmed.StateReplace => ZIO.unit
          case _ =>
            val actionType = action.getClass.getSimpleName.stripSuffix("$")
            val actor = extractLightningActor(action)
            confirmedActionRepository
              .append("LightningTalk", actionType, action.toJson, actor)
              .unit
      case HackathonProjectActionConfirmedMessage(action) =>
        action match
          case _: HackathonProjectActionConfirmed.Rejected     => ZIO.unit
          case _: HackathonProjectActionConfirmed.Unauthorized => ZIO.unit
          case _: HackathonProjectActionConfirmed.StateReplace => ZIO.unit
          case _ =>
            val actionType = action.getClass.getSimpleName.stripSuffix("$")
            val actor = extractHackathonActor(action)
            confirmedActionRepository
              .append("HackathonProject", actionType, action.toJson, actor)
              .unit
      case ActivityActionConfirmedMessage(action) =>
        action match
          case _: ActivityActionConfirmed.Rejected     => ZIO.unit
          case _: ActivityActionConfirmed.Unauthorized => ZIO.unit
          case _: ActivityActionConfirmed.StateReplace => ZIO.unit
          case _ =>
            val actionType = action.getClass.getSimpleName.stripSuffix("$")
            val actor = extractActivityActor(action)
            confirmedActionRepository
              .append("Activity", actionType, action.toJson, actor)
              .unit

  /** Extract actor (GitHub username) from discussion action. */
  private def extractDiscussionActor(action: DiscussionActionConfirmed): Option[String] =
    import neotype.unwrap
    action match
      case DiscussionActionConfirmed.Vote(_, feedback)     => Some(feedback.voter.unwrap)
      case DiscussionActionConfirmed.AddResult(discussion) => Some(discussion.facilitator.unwrap)
      case DiscussionActionConfirmed.ResetUser(person, _, _) => Some(person.unwrap)
      case _ => None

  /** Extract actor (GitHub username) from lightning talk action. */
  private def extractLightningActor(action: LightningTalkActionConfirmed): Option[String] =
    import neotype.unwrap
    action match
      case LightningTalkActionConfirmed.AddResult(proposal) => Some(proposal.speaker.unwrap)
      case _ => None

  /** Extract actor (GitHub username) from hackathon project action. */
  private def extractHackathonActor(action: HackathonProjectActionConfirmed): Option[String] =
    import neotype.unwrap
    action match
      case HackathonProjectActionConfirmed.Created(project)          => Some(project.owner.unwrap)
      case HackathonProjectActionConfirmed.Joined(_, person, _)      => Some(person.unwrap)
      case HackathonProjectActionConfirmed.Left(_, person, _)        => Some(person.unwrap)
      case HackathonProjectActionConfirmed.OwnershipTransferred(_, newOwner) => Some(newOwner.unwrap)
      case _ => None

  /** Extract actor (GitHub username) from activity action. */
  private def extractActivityActor(action: ActivityActionConfirmed): Option[String] =
    import neotype.unwrap
    action match
      case ActivityActionConfirmed.Created(activity) => Some(activity.creator.unwrap)
      case ActivityActionConfirmed.InterestSet(_, person, _, _, _) => Some(person.unwrap)
      case _ => None

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
          ZIO.service[DiscussionStore].run,
          ZIO.service[LightningTalkService].run,
          ZIO.service[HackathonProjectService].run,
          ZIO.service[ActivityService].run,
          ZIO.service[AuthenticatedTicketService].run,
          ZIO.service[AdminConfig].run,
          ZIO.service[SlackNotifier].run,
          ZIO.service[ConfirmedActionRepository].run,
          ZIO.service[UserRepository].run,
        )
