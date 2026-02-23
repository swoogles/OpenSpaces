package co.wtf.openspaces.slack

import co.wtf.openspaces.*
import co.wtf.openspaces.db.{DiscussionRepository, LightningTalkRepository, LightningTalkRow, HackathonProjectRepository, HackathonProjectRow}
import co.wtf.openspaces.hackathon.*
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

class SlackNotifierLive(
  slackClient: SlackClient,
  config: SlackConfig,
  discussionRepo: DiscussionRepository,
  lightningTalkRepo: LightningTalkRepository,
  hackathonProjectRepo: HackathonProjectRepository,
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

      case DiscussionActionConfirmed.SetRoomSlot(topicId, newRoomSlot) =>
        handleUpdateRoomSlot(topicId, newRoomSlot).fork.unit

      case DiscussionActionConfirmed.Delete(topicId) =>
        handleDelete(topicId).fork.unit

      case DiscussionActionConfirmed.SwapTopics(topic1, newSlot1, topic2, newSlot2) =>
        (handleUpdateRoomSlot(topic1, Some(newSlot1)) *> handleUpdateRoomSlot(topic2, Some(newSlot2))).fork.unit

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
      case HackathonProjectActionConfirmed.Joined(projectId, person, _) =>
        handleHackathonJoin(projectId, person).fork.unit
      case HackathonProjectActionConfirmed.Left(projectId, person, newOwner) =>
        handleHackathonLeave(projectId, person, newOwner).fork.unit
      case HackathonProjectActionConfirmed.Renamed(projectId, newTitle) =>
        handleHackathonRename(projectId, newTitle).fork.unit
      case HackathonProjectActionConfirmed.Deleted(projectId) =>
        handleHackathonDelete(projectId).fork.unit
      case _ =>
        ZIO.unit

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
      blocks = buildUpdateBlocks(row.copy(roomSlot = roomSlot.map(_.toJson)))
      _ <- slackClient.updateMessage(channelId, ts, blocks)
      // Also post a reply to the thread notifying about the schedule change
      replyText = roomSlot match
        case Some(rs) => s":round_pushpin: Scheduled: ${rs.room.name} @ ${rs.timeSlot.s}"
        case None => ":round_pushpin: This session has been unscheduled"
      _ <- ZIO.logInfo(s"Posting thread reply for topic $topicId: $replyText")
      _ <- slackClient.postReply(channelId, ts, replyText).catchAll { err =>
        ZIO.logError(s"Failed to post thread reply for topic $topicId: $err")
      }
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
      ref <- slackClient.postMessage(config.channelId, blocks).retry(retryPolicy)
      permalink <- slackClient.getPermalink(config.channelId, ref.ts).retry(retryPolicy)
      _ <- lightningTalkRepo.updateSlackThread(proposalId, config.channelId, ref.ts, permalink)
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
      channelId = row.slackChannelId.getOrElse(config.channelId)
      blocks = buildLightningUpdateBlocks(row)
      _ <- slackClient.updateMessage(channelId, ts, blocks)
      replyText = assignment match
        case Some(value) =>
          s":zap: Assigned: ${value.night.toString} Night slot #${value.slot.unwrap}"
        case None =>
          ":zap: Assignment cleared"
      _ <- slackClient.postReply(channelId, ts, replyText).catchAll { err =>
        ZIO.logError(s"Failed to post thread reply for lightning talk $proposalId: $err")
      }
    yield ()
    effect.catchAll(err => ZIO.logError(s"Slack assignment update failed for lightning talk $proposalId: $err"))

  private def handleLightningDelete(
    proposalId: LightningTalkId,
    slackChannelId: Option[String],
    slackThreadTs: Option[String],
  ): Task[Unit] =
    slackThreadTs match
      case Some(ts) =>
        val channelId = slackChannelId.getOrElse(config.channelId)
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
    val topicName = row.topic.replace("\"", "\\\"")
    val facilitator = row.facilitator.replace("\"", "\\\"")
    val avatarUrl = s"https://github.com/${row.facilitator}.png?size=100"
    val appLink = s"${config.appBaseUrl}"

    val scheduleInfo = row.roomSlot.flatMap(_.fromJson[RoomSlot].toOption) match
      case Some(rs) => s" · :round_pushpin: ${rs.room.name} · ${rs.timeSlot.s}"
      case None     => ""

    s"""[{"type":"section","text":{"type":"mrkdwn","text":"*$topicName*"},"accessory":{"type":"image","image_url":"$avatarUrl","alt_text":"$facilitator"}},{"type":"context","elements":[{"type":"mrkdwn","text":"Proposed by *$facilitator*$scheduleInfo · <$appLink|View in OpenSpaces>"}]}]"""

  private def buildLightningCreateBlocks(proposal: LightningTalkProposal): String =
    val speaker = proposal.speaker.unwrap.replace("\"", "\\\"")
    val avatarUrl = s"https://github.com/${proposal.speaker.unwrap}.png?size=100"
    val appLink = s"${config.appBaseUrl}"

    s"""[{"type":"section","text":{"type":"mrkdwn","text":":zap: *Lightning Talk Volunteer*"},"accessory":{"type":"image","image_url":"$avatarUrl","alt_text":"$speaker"}},{"type":"context","elements":[{"type":"mrkdwn","text":"*${speaker}* is willing to give a lightning talk · <$appLink|View in OpenSpaces>"}]}]"""

  private def buildLightningUpdateBlocks(row: LightningTalkRow): String =
    val speaker = row.speaker.replace("\"", "\\\"")
    val avatarUrl = s"https://github.com/${row.speaker}.png?size=100"
    val appLink = s"${config.appBaseUrl}"
    val assignmentInfo = (row.assignmentNight, row.assignmentSlot) match
      case (Some(night), Some(slot)) => s" · :zap: $night Night slot #$slot"
      case _ => ""
    s"""[{"type":"section","text":{"type":"mrkdwn","text":":zap: *Lightning Talk Volunteer*"},"accessory":{"type":"image","image_url":"$avatarUrl","alt_text":"$speaker"}},{"type":"context","elements":[{"type":"mrkdwn","text":"*${speaker}* is willing to give a lightning talk$assignmentInfo · <$appLink|View in OpenSpaces>"}]}]"""

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

object SlackNotifier:
  val layer: ZLayer[Option[SlackConfigEnv] & DiscussionRepository & LightningTalkRepository & HackathonProjectRepository & Client, Nothing, SlackNotifier] =
    ZLayer.fromZIO:
      for
        maybeEnvConfig <- ZIO.service[Option[SlackConfigEnv]]
        discussionRepo <- ZIO.service[DiscussionRepository]
        lightningTalkRepo <- ZIO.service[LightningTalkRepository]
        hackathonProjectRepo <- ZIO.service[HackathonProjectRepository]
        client         <- ZIO.service[Client]
        notifier <- maybeEnvConfig match
          case Some(envConfig) =>
            val effect = for
              channelId <- resolveOrCreateChannel(client, envConfig.botToken, envConfig.channelName)
              hackathonChannelId <- resolveOrCreateChannel(client, envConfig.botToken, envConfig.hackathonChannelName)
              config = SlackConfig(
                envConfig.botToken,
                channelId,
                envConfig.channelName,
                hackathonChannelId,
                envConfig.hackathonChannelName,
                envConfig.appBaseUrl,
              )
              _ <- ZIO.logInfo(s"Slack integration enabled for channels #${envConfig.channelName} ($channelId) and #${envConfig.hackathonChannelName} ($hackathonChannelId)")
            yield SlackNotifierLive(SlackClientLive(client, config), config, discussionRepo, lightningTalkRepo, hackathonProjectRepo)
            
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
    val tempConfig = SlackConfig(botToken, "", "", "", "", "")
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
