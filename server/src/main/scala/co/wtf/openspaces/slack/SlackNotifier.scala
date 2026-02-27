package co.wtf.openspaces.slack

import co.wtf.openspaces.*
import co.wtf.openspaces.db.{DiscussionRepository, LightningTalkRepository, LightningTalkRow, HackathonProjectRepository, HackathonProjectRow}
import co.wtf.openspaces.discussions.{Discussion, DiscussionActionConfirmed}
import co.wtf.openspaces.hackathon.*
import co.wtf.openspaces.lighting_talks.*
import co.wtf.openspaces.activities.*
import neotype.unwrap
import zio.*
import zio.http.Client
import zio.json.*

trait SlackNotifier:
  def notifyDiscussion(
    action: DiscussionActionConfirmed,
    broadcast: DiscussionActionConfirmed => Task[Unit],
  ): Task[Unit]
  def notifyLightning(
    action: LightningTalkActionConfirmed,
    broadcast: LightningTalkActionConfirmed => Task[Unit],
  ): Task[Unit]
  def notifyHackathonProject(
    action: HackathonProjectActionConfirmed,
    broadcast: HackathonProjectActionConfirmed => Task[Unit],
  ): Task[Unit]
  def notifyActivity(
    action: ActivityActionConfirmed,
    broadcast: ActivityActionConfirmed => Task[Unit],
  ): Task[Unit]
  def notifyAccessRequest(
    username: String,
    displayName: Option[String],
  ): Task[Unit]

class SlackNotifierLive(
  slackClient: SlackClient,
  config: SlackConfig,
  discussionRepo: DiscussionRepository,
  lightningTalkRepo: LightningTalkRepository,
  hackathonProjectRepo: HackathonProjectRepository,
  activityRepo: co.wtf.openspaces.db.ActivityRepository,
) extends SlackNotifier:

  private val retryPolicy = Schedule.exponential(1.second) && Schedule.recurs(2)

  def notifyDiscussion(
    action: DiscussionActionConfirmed,
    broadcast: DiscussionActionConfirmed => Task[Unit],
  ): Task[Unit] =
    action match
      case DiscussionActionConfirmed.AddResult(discussion) =>
        handleAdd(discussion, broadcast).fork.unit

      case DiscussionActionConfirmed.Rename(topicId, newTopic) =>
        handleRename(topicId, newTopic).fork.unit

      case DiscussionActionConfirmed.SetRoomSlot(_, _) =>
        ZIO.unit

      case DiscussionActionConfirmed.Delete(topicId) =>
        handleDelete(topicId).fork.unit

      case DiscussionActionConfirmed.SwapTopics(_, _, _, _) =>
        ZIO.unit

      case _ => ZIO.unit // Vote, SlackThreadLinked, Rejected are no-ops

  def notifyLightning(
    action: LightningTalkActionConfirmed,
    broadcast: LightningTalkActionConfirmed => Task[Unit],
  ): Task[Unit] =
    action match
      case LightningTalkActionConfirmed.AddResult(proposal) =>
        handleLightningAdd(proposal, broadcast).fork.unit
      case LightningTalkActionConfirmed.Delete(proposalId, slackChannelId, slackThreadTs) =>
        handleLightningDelete(proposalId, slackChannelId, slackThreadTs).fork.unit
      case LightningTalkActionConfirmed.SetAssignment(proposalId, newAssignment) =>
        handleLightningAssignmentUpdate(proposalId, newAssignment).fork.unit
      case LightningTalkActionConfirmed.DrawForNightResult(_, assignments) =>
        ZIO.foreachDiscard(assignments) { assignment =>
          handleLightningAssignmentUpdate(assignment.proposalId, Some(assignment.assignment))
        }.fork.unit
      case _ =>
        ZIO.unit

  def notifyHackathonProject(
    action: HackathonProjectActionConfirmed,
    broadcast: HackathonProjectActionConfirmed => Task[Unit],
  ): Task[Unit] =
    action match
      case HackathonProjectActionConfirmed.Created(project) =>
        handleHackathonCreate(project, broadcast).fork.unit
      case HackathonProjectActionConfirmed.Joined(_, _, _) =>
        ZIO.unit
      case HackathonProjectActionConfirmed.Left(_, _, _) =>
        ZIO.unit
      case HackathonProjectActionConfirmed.Renamed(projectId, newTitle) =>
        handleHackathonRename(projectId, newTitle).fork.unit
      case HackathonProjectActionConfirmed.Deleted(projectId) =>
        handleHackathonDelete(projectId).fork.unit
      case _ =>
        ZIO.unit

  def notifyActivity(
    action: ActivityActionConfirmed,
    broadcast: ActivityActionConfirmed => Task[Unit],
  ): Task[Unit] =
    action match
      case ActivityActionConfirmed.Created(activity) =>
        handleActivityCreate(activity, broadcast).fork.unit
      case ActivityActionConfirmed.Updated(activityId, newDescription, newEventTime) =>
        handleActivityUpdate(activityId, newDescription, newEventTime).fork.unit
      case ActivityActionConfirmed.Deleted(activityId, slackChannelId, slackThreadTs) =>
        handleActivityDelete(activityId, slackChannelId, slackThreadTs).fork.unit
      case _ =>
        ZIO.unit

  def notifyAccessRequest(
    username: String,
    displayName: Option[String],
  ): Task[Unit] =
    val effect = for
      _ <- ZIO.logInfo(s"Sending access request notification for user: $username")
      blocks = buildAccessRequestBlocks(username, displayName)
      _ <- slackClient.postMessage(config.accessRequestChannelId, blocks).retry(retryPolicy)
    yield ()
    
    effect
      .catchAll(err => ZIO.logError(s"Failed to send access request notification for $username: $err"))
      .fork
      .unit

  private def buildAccessRequestBlocks(username: String, displayName: Option[String]): String =
    val escapedUsername = username.replace("\"", "\\\"")
    val escapedDisplayName = displayName.map(_.replace("\"", "\\\"")).getOrElse(escapedUsername)
    val avatarUrl = s"https://github.com/$username.png?size=100"
    val appLink = s"${config.appBaseUrl}"
    s"""[{"type":"section","text":{"type":"mrkdwn","text":":wave: *New Access Request*\\n*$escapedDisplayName* (@$escapedUsername) is requesting access to OpenSpaces."},"accessory":{"type":"image","image_url":"$avatarUrl","alt_text":"$escapedDisplayName"}},{"type":"context","elements":[{"type":"mrkdwn","text":"<https://github.com/$escapedUsername|GitHub Profile> · <$appLink|Open Admin Panel>"}]}]"""

  private def handleAdd(discussion: Discussion, broadcast: DiscussionActionConfirmed => Task[Unit]): Task[Unit] =
    val blocks = buildCreateBlocks(discussion)
    val topicId = discussion.id.unwrap

    val effect = for
      ref       <- slackClient.postMessage(config.channelId, blocks).retry(retryPolicy)
      permalink <- slackClient.getPermalink(config.channelId, ref.ts).retry(retryPolicy)
      _         <- discussionRepo.updateSlackThread(topicId, config.channelId, ref.ts, permalink)
      _         <- broadcast(DiscussionActionConfirmed.SlackThreadLinked(discussion.id, permalink))
    yield ()

    effect.catchAll(err => ZIO.logError(s"Slack integration failed for topic $topicId: $err"))

  private def handleRename(topicId: TopicId, newTopic: Topic): Task[Unit] =
    val effect = for
      row <- discussionRepo.findById(topicId.unwrap).someOrFail(new Exception(s"Discussion $topicId not found"))
      ts  <- ZIO.fromOption(row.slackThreadTs).orElseFail(new Exception(s"No Slack thread for topic $topicId"))
      // Use the channel where the thread was created, not the current config channel
      channelId = row.slackChannelId.getOrElse(config.channelId)
      blocks = buildUpdateBlocks(row.copy(topic = newTopic.unwrap))
      _ <- slackClient.updateMessage(channelId, ts, blocks)
    yield ()

    effect.catchAll(err => ZIO.logError(s"Slack rename failed for topic $topicId: $err"))

  private def handleUpdateRoomSlot(topicId: TopicId, roomSlot: Option[RoomSlot]): Task[Unit] =
    val effect = for
      row <- discussionRepo.findById(topicId.unwrap).someOrFail(new Exception(s"Discussion $topicId not found"))
      ts  <- ZIO.fromOption(row.slackThreadTs).orElseFail(new Exception(s"No Slack thread for topic $topicId"))
      // Use the channel where the thread was created, not the current config channel
      channelId = row.slackChannelId.getOrElse(config.channelId)
      blocks = buildUpdateBlocksWithSlot(row, roomSlot)
      _ <- slackClient.updateMessage(channelId, ts, blocks)
    yield ()

    effect.catchAll(err => ZIO.logError(s"Slack schedule update failed for topic $topicId: $err"))

  private def handleDelete(topicId: TopicId): Task[Unit] =
    val effect = for
      row <- discussionRepo.findById(topicId.unwrap).someOrFail(new Exception(s"Discussion $topicId not found"))
      ts  <- ZIO.fromOption(row.slackThreadTs).orElseFail(new Exception(s"No Slack thread for topic $topicId"))
      // Use the channel where the thread was created, not the current config channel
      channelId = row.slackChannelId.getOrElse(config.channelId)
      _   <- slackClient.deleteMessage(channelId, ts)
    yield ()

    effect.catchAll(err => ZIO.logError(s"Slack delete failed for topic $topicId: $err"))

  private def handleLightningAdd(
    proposal: LightningTalkProposal,
    broadcast: LightningTalkActionConfirmed => Task[Unit],
  ): Task[Unit] =
    val blocks = buildLightningCreateBlocks(proposal)
    val proposalId = proposal.id.unwrap
    val effect = for
      ref <- slackClient.postMessage(config.lightningChannelId, blocks).retry(retryPolicy)
      permalink <- slackClient.getPermalink(config.lightningChannelId, ref.ts).retry(retryPolicy)
      _ <- lightningTalkRepo.updateSlackThread(proposalId, config.lightningChannelId, ref.ts, permalink)
      _ <- broadcast(LightningTalkActionConfirmed.SlackThreadLinked(proposal.id, permalink))
    yield ()
    effect.catchAll(err => ZIO.logError(s"Slack integration failed for lightning proposal $proposalId: $err"))

  private def handleLightningAssignmentUpdate(
    proposalId: LightningTalkId,
    assignment: Option[LightningAssignment],
  ): Task[Unit] =
    val effect = for
      row <- lightningTalkRepo.findById(proposalId.unwrap).someOrFail(new Exception(s"Lightning talk $proposalId not found"))
      ts <- ZIO.fromOption(row.slackThreadTs).orElseFail(new Exception(s"No Slack thread for lightning talk $proposalId"))
      channelId = row.slackChannelId.getOrElse(config.lightningChannelId)
      blocks = buildLightningUpdateBlocks(row)
      _ <- slackClient.updateMessage(channelId, ts, blocks)
    yield ()
    effect.catchAll(err => ZIO.logError(s"Slack assignment update failed for lightning talk $proposalId: $err"))

  private def handleLightningDelete(
    proposalId: LightningTalkId,
    slackChannelId: Option[String],
    slackThreadTs: Option[String],
  ): Task[Unit] =
    slackThreadTs match
      case Some(ts) =>
        val channelId = slackChannelId.getOrElse(config.lightningChannelId)
        slackClient
          .deleteMessage(channelId, ts)
          .catchAll(err => ZIO.logError(s"Slack delete failed for lightning talk $proposalId: $err"))
      case None =>
        ZIO.unit

  private def buildCreateBlocks(discussion: Discussion): String =
    val topicName = discussion.topicName.replace("\"", "\\\"")
    val facilitator = discussion.facilitatorName.replace("\"", "\\\"")
    val avatarUrl = s"https://github.com/${discussion.facilitator.unwrap}.png?size=100"
    val appLink = s"${config.appBaseUrl}"

    s"""[{"type":"section","text":{"type":"mrkdwn","text":"*$topicName*"},"accessory":{"type":"image","image_url":"$avatarUrl","alt_text":"$facilitator"}},{"type":"context","elements":[{"type":"mrkdwn","text":"Proposed by *$facilitator* · <$appLink|View in OpenSpaces>"}]}]"""

  private def buildUpdateBlocks(row: co.wtf.openspaces.db.DiscussionRow): String =
    // For rename operations, we don't have the room slot info readily available
    // Just show the topic without schedule info
    buildUpdateBlocksWithSlot(row, None)

  private def buildUpdateBlocksWithSlot(row: co.wtf.openspaces.db.DiscussionRow, roomSlot: Option[RoomSlot]): String =
    val topicName = row.topic.replace("\"", "\\\"")
    val facilitator = row.facilitator.replace("\"", "\\\"")
    val avatarUrl = s"https://github.com/${row.facilitator}.png?size=100"
    val appLink = s"${config.appBaseUrl}"

    val scheduleInfo = roomSlot match
      case Some(rs) => s" · :round_pushpin: ${rs.room.name} · ${escapeColonsForSlack(rs.timeSlot.displayString)}"
      case None     => ""

    s"""[{"type":"section","text":{"type":"mrkdwn","text":"*$topicName*"},"accessory":{"type":"image","image_url":"$avatarUrl","alt_text":"$facilitator"}},{"type":"context","elements":[{"type":"mrkdwn","text":"Proposed by *$facilitator*$scheduleInfo · <$appLink|View in OpenSpaces>"}]}]"""

  private def buildLightningCreateBlocks(proposal: LightningTalkProposal): String =
    val speaker = proposal.speaker.unwrap.replace("\"", "\\\"")
    val avatarUrl = s"https://github.com/${proposal.speaker.unwrap}.png?size=100"
    val appLink = s"${config.appBaseUrl}"

    s"""[{"type":"section","text":{"type":"mrkdwn","text":":zap: *$speaker Lightning talk*"},"accessory":{"type":"image","image_url":"$avatarUrl","alt_text":"$speaker"}},{"type":"context","elements":[{"type":"mrkdwn","text":"*${speaker}* is willing to give a lightning talk · <$appLink|View in OpenSpaces>"}]}]"""

  private def buildLightningUpdateBlocks(row: LightningTalkRow): String =
    val speaker = row.speaker.replace("\"", "\\\"")
    val avatarUrl = s"https://github.com/${row.speaker}.png?size=100"
    val appLink = s"${config.appBaseUrl}"
    val assignmentInfo = (row.assignmentNight, row.assignmentSlot) match
      case (Some(night), Some(slot)) => s" · :zap: $night Night slot #$slot"
      case _ => ""
    s"""[{"type":"section","text":{"type":"mrkdwn","text":":zap: *$speaker Lightning talk*"},"accessory":{"type":"image","image_url":"$avatarUrl","alt_text":"$speaker"}},{"type":"context","elements":[{"type":"mrkdwn","text":"*${speaker}* is willing to give a lightning talk$assignmentInfo · <$appLink|View in OpenSpaces>"}]}]"""

  // Hackathon Project handlers

  private def handleHackathonCreate(
    project: HackathonProject,
    broadcast: HackathonProjectActionConfirmed => Task[Unit],
  ): Task[Unit] =
    val blocks = buildHackathonCreateBlocks(project)
    val projectId = project.id.unwrap

    val effect = for
      ref       <- slackClient.postMessage(config.hackathonChannelId, blocks).retry(retryPolicy)
      permalink <- slackClient.getPermalink(config.hackathonChannelId, ref.ts).retry(retryPolicy)
      _         <- hackathonProjectRepo.updateSlackThread(projectId, config.hackathonChannelId, ref.ts, permalink)
      _         <- broadcast(HackathonProjectActionConfirmed.SlackThreadLinked(project.id, permalink))
    yield ()

    effect.catchAll(err => ZIO.logError(s"Slack integration failed for hackathon project $projectId: $err"))

  private def handleHackathonJoin(projectId: HackathonProjectId, person: Person): Task[Unit] =
    val effect = for
      row <- hackathonProjectRepo.findById(projectId.unwrap).someOrFail(new Exception(s"Project $projectId not found"))
      ts  <- ZIO.fromOption(row.slackThreadTs).orElseFail(new Exception(s"No Slack thread for project $projectId"))
      channelId = row.slackChannelId.getOrElse(config.hackathonChannelId)
      replyText = s":wave: *${person.unwrap}* joined the project"
      _ <- slackClient.postReply(channelId, ts, replyText)
    yield ()

    effect.catchAll(err => ZIO.logError(s"Slack join notification failed for project $projectId: $err"))

  private def handleHackathonLeave(
    projectId: HackathonProjectId,
    person: Person,
    newOwner: Option[Person],
  ): Task[Unit] =
    val effect = for
      row <- hackathonProjectRepo.findById(projectId.unwrap).someOrFail(new Exception(s"Project $projectId not found"))
      ts  <- ZIO.fromOption(row.slackThreadTs).orElseFail(new Exception(s"No Slack thread for project $projectId"))
      channelId = row.slackChannelId.getOrElse(config.hackathonChannelId)
      leaveText = s":wave: *${person.unwrap}* left the project"
      _ <- slackClient.postReply(channelId, ts, leaveText)
      _ <- newOwner match
        case Some(owner) =>
          slackClient.postReply(channelId, ts, s":key: *${owner.unwrap}* is now leading this project")
        case None =>
          ZIO.unit
    yield ()

    effect.catchAll(err => ZIO.logError(s"Slack leave notification failed for project $projectId: $err"))

  private def handleHackathonRename(projectId: HackathonProjectId, newTitle: ProjectTitle): Task[Unit] =
    val effect = for
      row <- hackathonProjectRepo.findById(projectId.unwrap).someOrFail(new Exception(s"Project $projectId not found"))
      ts  <- ZIO.fromOption(row.slackThreadTs).orElseFail(new Exception(s"No Slack thread for project $projectId"))
      channelId = row.slackChannelId.getOrElse(config.hackathonChannelId)
      blocks = buildHackathonUpdateBlocks(row.copy(title = newTitle.unwrap))
      _ <- slackClient.updateMessage(channelId, ts, blocks)
    yield ()

    effect.catchAll(err => ZIO.logError(s"Slack rename failed for hackathon project $projectId: $err"))

  private def handleHackathonDelete(projectId: HackathonProjectId): Task[Unit] =
    val effect = for
      row <- hackathonProjectRepo.findById(projectId.unwrap).someOrFail(new Exception(s"Project $projectId not found"))
      ts  <- ZIO.fromOption(row.slackThreadTs).orElseFail(new Exception(s"No Slack thread for project $projectId"))
      channelId = row.slackChannelId.getOrElse(config.hackathonChannelId)
      _ <- slackClient.deleteMessage(channelId, ts)
    yield ()

    effect.catchAll(err => ZIO.logError(s"Slack delete failed for hackathon project $projectId: $err"))

  private def buildHackathonCreateBlocks(project: HackathonProject): String =
    val title = project.title.unwrap.replace("\"", "\\\"")
    val owner = project.ownerName.replace("\"", "\\\"")
    val avatarUrl = s"https://github.com/${project.owner.unwrap}.png?size=100"
    val appLink = s"${config.appBaseUrl}"

    s"""[{"type":"section","text":{"type":"mrkdwn","text":":hammer_and_wrench: *$title*"},"accessory":{"type":"image","image_url":"$avatarUrl","alt_text":"$owner"}},{"type":"context","elements":[{"type":"mrkdwn","text":"Proposed by *$owner* · <$appLink|View in OpenSpaces>"}]}]"""

  private def buildHackathonUpdateBlocks(row: HackathonProjectRow): String =
    val title = row.title.replace("\"", "\\\"")
    val owner = row.owner.replace("\"", "\\\"")
    val avatarUrl = s"https://github.com/${row.owner}.png?size=100"
    val appLink = s"${config.appBaseUrl}"

    s"""[{"type":"section","text":{"type":"mrkdwn","text":":hammer_and_wrench: *$title*"},"accessory":{"type":"image","image_url":"$avatarUrl","alt_text":"$owner"}},{"type":"context","elements":[{"type":"mrkdwn","text":"Proposed by *$owner* · <$appLink|View in OpenSpaces>"}]}]"""

  // Activity handlers

  private def handleActivityCreate(
    activity: Activity,
    broadcast: ActivityActionConfirmed => Task[Unit],
  ): Task[Unit] =
    val blocks = buildActivityCreateBlocks(activity)
    val activityId = activity.id.unwrap
    val effect = for
      ref <- slackClient.postMessage(config.activityChannelId, blocks).retry(retryPolicy)
      permalink <- slackClient.getPermalink(config.activityChannelId, ref.ts).retry(retryPolicy)
      _ <- activityRepo.updateSlackThread(activityId, config.activityChannelId, ref.ts, permalink)
      _ <- broadcast(ActivityActionConfirmed.SlackThreadLinked(activity.id, permalink))
    yield ()
    effect.catchAll(err => ZIO.logError(s"Slack integration failed for activity $activityId: $err"))

  private def handleActivityUpdate(
    activityId: ActivityId,
    newDescription: ActivityDescription,
    newEventTime: java.time.LocalDateTime,
  ): Task[Unit] =
    val effect = for
      row <- activityRepo.findById(activityId.unwrap).someOrFail(new Exception(s"Activity $activityId not found"))
      _ <- ZIO.when(row.description != newDescription.unwrap) {
        for
          ts <- ZIO.fromOption(row.slackThreadTs).orElseFail(new Exception(s"No Slack thread for activity $activityId"))
          channelId = row.slackChannelId.getOrElse(config.activityChannelId)
          blocks = buildActivityUpdateBlocks(row.copy(description = newDescription.unwrap, eventTime = newEventTime))
          _ <- slackClient.updateMessage(channelId, ts, blocks)
        yield ()
      }
    yield ()
    effect.catchAll(err => ZIO.logError(s"Slack update failed for activity $activityId: $err"))

  private def handleActivityDelete(
    activityId: ActivityId,
    slackChannelId: Option[String],
    slackThreadTs: Option[String],
  ): Task[Unit] =
    slackThreadTs match
      case Some(ts) =>
        val channelId = slackChannelId.getOrElse(config.activityChannelId)
        slackClient
          .deleteMessage(channelId, ts)
          .catchAll(err => ZIO.logError(s"Slack delete failed for activity $activityId: $err"))
      case None =>
        ZIO.unit

  private def buildActivityCreateBlocks(activity: Activity): String =
    val description = activity.descriptionText.replace("\"", "\\\"")
    val creator = activity.creatorName.replace("\"", "\\\"")
    val avatarUrl = s"https://github.com/${activity.creator.unwrap}.png?size=100"
    val timeString = activity.eventTime.toString.replace("T", " ")
    val appLink = s"${config.appBaseUrl}"
    s"""[{"type":"section","text":{"type":"mrkdwn","text":":calendar: *$description*"},"accessory":{"type":"image","image_url":"$avatarUrl","alt_text":"$creator"}},{"type":"context","elements":[{"type":"mrkdwn","text":"Proposed by *$creator* · $timeString · <$appLink|View in OpenSpaces>"}]}]"""

  private def buildActivityUpdateBlocks(row: co.wtf.openspaces.db.ActivityRow): String =
    val description = row.description.replace("\"", "\\\"")
    val creator = row.creator.replace("\"", "\\\"")
    val avatarUrl = s"https://github.com/${row.creator}.png?size=100"
    val appLink = s"${config.appBaseUrl}"
    s"""[{"type":"section","text":{"type":"mrkdwn","text":":calendar: *$description*"},"accessory":{"type":"image","image_url":"$avatarUrl","alt_text":"$creator"}},{"type":"context","elements":[{"type":"mrkdwn","text":"Proposed by *$creator* · <$appLink|View in OpenSpaces>"}]}]"""

  // Replace colons with Unicode ratio character (∶ U+2236) to prevent Slack emoji parsing
  private def escapeColonsForSlack(s: String): String = s.replace(":", "∶")


class SlackNotifierNoOp extends SlackNotifier:
  def notifyDiscussion(
    action: DiscussionActionConfirmed,
    broadcast: DiscussionActionConfirmed => Task[Unit],
  ): Task[Unit] =
    ZIO.unit
  def notifyLightning(
    action: LightningTalkActionConfirmed,
    broadcast: LightningTalkActionConfirmed => Task[Unit],
  ): Task[Unit] =
    ZIO.unit
  def notifyHackathonProject(
    action: HackathonProjectActionConfirmed,
    broadcast: HackathonProjectActionConfirmed => Task[Unit],
  ): Task[Unit] =
    ZIO.unit
  def notifyActivity(
    action: ActivityActionConfirmed,
    broadcast: ActivityActionConfirmed => Task[Unit],
  ): Task[Unit] =
    ZIO.unit
  def notifyAccessRequest(
    username: String,
    displayName: Option[String],
  ): Task[Unit] =
    ZIO.unit

object SlackNotifier:
  val layer: ZLayer[Option[SlackConfigEnv] & DiscussionRepository & LightningTalkRepository & HackathonProjectRepository & co.wtf.openspaces.db.ActivityRepository & Client, Nothing, SlackNotifier] =
    ZLayer.fromZIO:
      for
        maybeEnvConfig <- ZIO.service[Option[SlackConfigEnv]]
        discussionRepo <- ZIO.service[DiscussionRepository]
        lightningTalkRepo <- ZIO.service[LightningTalkRepository]
        hackathonProjectRepo <- ZIO.service[HackathonProjectRepository]
        activityRepo <- ZIO.service[co.wtf.openspaces.db.ActivityRepository]
        client         <- ZIO.service[Client]
        notifier <- maybeEnvConfig match
          case Some(envConfig) =>
            val effect = for
              channelId <- resolveOrCreateChannel(client, envConfig.botToken, envConfig.channelName)
              lightningChannelId <- resolveOrCreateChannel(client, envConfig.botToken, envConfig.lightningChannelName)
              activityChannelId <- resolveOrCreateChannel(client, envConfig.botToken, envConfig.activityChannelName)
              hackathonChannelId <- resolveOrCreateChannel(client, envConfig.botToken, envConfig.hackathonChannelName)
              accessRequestChannelId <- resolveOrCreateChannel(client, envConfig.botToken, envConfig.accessRequestChannelName)
              config = SlackConfig(
                envConfig.botToken,
                channelId,
                envConfig.channelName,
                lightningChannelId,
                envConfig.lightningChannelName,
                activityChannelId,
                envConfig.activityChannelName,
                hackathonChannelId,
                envConfig.hackathonChannelName,
                accessRequestChannelId,
                envConfig.accessRequestChannelName,
                envConfig.appBaseUrl,
              )
              _ <- ZIO.logInfo(
                s"Slack integration enabled for channels #${envConfig.channelName} ($channelId), #${envConfig.lightningChannelName} ($lightningChannelId), #${envConfig.activityChannelName} ($activityChannelId), #${envConfig.hackathonChannelName} ($hackathonChannelId), #${envConfig.accessRequestChannelName} ($accessRequestChannelId)"
              )
            yield SlackNotifierLive(
              SlackClientLive(client, config),
              config,
              discussionRepo,
              lightningTalkRepo,
              hackathonProjectRepo,
              activityRepo,
            )
            
            effect.catchAll { err =>
              ZIO.logError(s"Failed to initialize Slack channels: $err") *>
                ZIO.succeed(SlackNotifierNoOp())
            }
          case None =>
            ZIO.logInfo("Slack integration disabled (SLACK_BOT_TOKEN or APP_BASE_URL not set)") *>
              ZIO.succeed(SlackNotifierNoOp())
      yield notifier

  private def resolveOrCreateChannel(client: Client, botToken: String, channelName: String): Task[String] =
    // Create a temporary SlackClient just for channel resolution
    val tempConfig = SlackConfig(botToken, "", "", "", "", "", "", "", "", "", "", "")
    val slackClient = SlackClientLive(client, tempConfig)
    
    for
      maybeChannelId <- slackClient.findChannelByName(channelName)
      channelId <- maybeChannelId match
        case Some(id) =>
          ZIO.logInfo(s"Found existing Slack channel #$channelName ($id)") *>
            ZIO.succeed(id)
        case None =>
          ZIO.logInfo(s"Creating Slack channel #$channelName...") *>
            slackClient.createChannel(channelName)
    yield channelId
