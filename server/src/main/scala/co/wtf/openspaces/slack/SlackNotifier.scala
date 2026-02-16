package co.wtf.openspaces.slack

import co.wtf.openspaces.*
import co.wtf.openspaces.db.{DiscussionRepository, LightningTalkRepository, LightningTalkRow}
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

class SlackNotifierLive(
  slackClient: SlackClient,
  config: SlackConfig,
  discussionRepo: DiscussionRepository,
  lightningTalkRepo: LightningTalkRepository,
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
      case LightningTalkActionConfirmed.Rename(proposalId, _) =>
        handleLightningRename(proposalId).fork.unit
      case LightningTalkActionConfirmed.Delete(proposalId) =>
        handleLightningDelete(proposalId).fork.unit
      case LightningTalkActionConfirmed.SetAssignment(proposalId, newAssignment) =>
        handleLightningAssignmentUpdate(proposalId, newAssignment).fork.unit
      case LightningTalkActionConfirmed.DrawForNightResult(_, assignments) =>
        ZIO.foreachDiscard(assignments) { assignment =>
          handleLightningAssignmentUpdate(assignment.proposalId, Some(assignment.assignment))
        }.fork.unit
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

  private def handleLightningRename(proposalId: LightningTalkId): Task[Unit] =
    val effect = for
      row <- lightningTalkRepo.findById(proposalId.unwrap).someOrFail(new Exception(s"Lightning talk $proposalId not found"))
      ts <- ZIO.fromOption(row.slackThreadTs).orElseFail(new Exception(s"No Slack thread for lightning talk $proposalId"))
      channelId = row.slackChannelId.getOrElse(config.channelId)
      blocks = buildLightningUpdateBlocks(row)
      _ <- slackClient.updateMessage(channelId, ts, blocks)
    yield ()
    effect.catchAll(err => ZIO.logError(s"Slack rename failed for lightning talk $proposalId: $err"))

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

  private def handleLightningDelete(proposalId: LightningTalkId): Task[Unit] =
    val effect = for
      row <- lightningTalkRepo.findById(proposalId.unwrap).someOrFail(new Exception(s"Lightning talk $proposalId not found"))
      ts <- ZIO.fromOption(row.slackThreadTs).orElseFail(new Exception(s"No Slack thread for lightning talk $proposalId"))
      channelId = row.slackChannelId.getOrElse(config.channelId)
      _ <- slackClient.deleteMessage(channelId, ts)
    yield ()
    effect.catchAll(err => ZIO.logError(s"Slack delete failed for lightning talk $proposalId: $err"))

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
    val topicName = proposal.topicName.replace("\"", "\\\"")
    val speaker = proposal.speaker.unwrap.replace("\"", "\\\"")
    val avatarUrl = s"https://github.com/${proposal.speaker.unwrap}.png?size=100"
    val appLink = s"${config.appBaseUrl}"

    s"""[{"type":"section","text":{"type":"mrkdwn","text":":zap: *$topicName*"},"accessory":{"type":"image","image_url":"$avatarUrl","alt_text":"$speaker"}},{"type":"context","elements":[{"type":"mrkdwn","text":"Lightning talk by *$speaker* · <$appLink|View in OpenSpaces>"}]}]"""

  private def buildLightningUpdateBlocks(row: LightningTalkRow): String =
    val topicName = row.topic.replace("\"", "\\\"")
    val speaker = row.speaker.replace("\"", "\\\"")
    val avatarUrl = s"https://github.com/${row.speaker}.png?size=100"
    val appLink = s"${config.appBaseUrl}"
    val assignmentInfo = (row.assignmentNight, row.assignmentSlot) match
      case (Some(night), Some(slot)) => s" · :zap: $night Night slot #$slot"
      case _ => ""
    s"""[{"type":"section","text":{"type":"mrkdwn","text":":zap: *$topicName*"},"accessory":{"type":"image","image_url":"$avatarUrl","alt_text":"$speaker"}},{"type":"context","elements":[{"type":"mrkdwn","text":"Lightning talk by *$speaker*$assignmentInfo · <$appLink|View in OpenSpaces>"}]}]"""


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

object SlackNotifier:
  val layer: ZLayer[Option[SlackConfigEnv] & DiscussionRepository & LightningTalkRepository & Client, Nothing, SlackNotifier] =
    ZLayer.fromZIO:
      for
        maybeEnvConfig <- ZIO.service[Option[SlackConfigEnv]]
        discussionRepo <- ZIO.service[DiscussionRepository]
        lightningTalkRepo <- ZIO.service[LightningTalkRepository]
        client         <- ZIO.service[Client]
        notifier <- maybeEnvConfig match
          case Some(envConfig) =>
            resolveOrCreateChannel(client, envConfig).flatMap { channelId =>
              val config = SlackConfig(envConfig.botToken, channelId, envConfig.channelName, envConfig.appBaseUrl)
              ZIO.logInfo(s"Slack integration enabled for channel #${envConfig.channelName} ($channelId)") *>
                ZIO.succeed(SlackNotifierLive(SlackClientLive(client, config), config, discussionRepo, lightningTalkRepo))
            }.catchAll { err =>
              ZIO.logError(s"Failed to initialize Slack channel: $err") *>
                ZIO.succeed(SlackNotifierNoOp())
            }
          case None =>
            ZIO.logInfo("Slack integration disabled (SLACK_BOT_TOKEN or APP_BASE_URL not set)") *>
              ZIO.succeed(SlackNotifierNoOp())
      yield notifier

  private def resolveOrCreateChannel(client: Client, envConfig: SlackConfigEnv): Task[String] =
    // Create a temporary SlackClient just for channel resolution
    val tempConfig = SlackConfig(envConfig.botToken, "", envConfig.channelName, envConfig.appBaseUrl)
    val slackClient = SlackClientLive(client, tempConfig)
    
    for
      maybeChannelId <- slackClient.findChannelByName(envConfig.channelName)
      channelId <- maybeChannelId match
        case Some(id) =>
          ZIO.logInfo(s"Found existing Slack channel #${envConfig.channelName} ($id)") *>
            ZIO.succeed(id)
        case None =>
          ZIO.logInfo(s"Creating Slack channel #${envConfig.channelName}...") *>
            slackClient.createChannel(envConfig.channelName)
    yield channelId
