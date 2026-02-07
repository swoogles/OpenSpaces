package co.wtf.openspaces.slack

import co.wtf.openspaces.*
import co.wtf.openspaces.db.DiscussionRepository
import neotype.unwrap
import zio.*
import zio.http.Client
import zio.json.*

trait SlackNotifier:
  def notify(action: DiscussionActionConfirmed, broadcast: DiscussionActionConfirmed => Task[Unit]): Task[Unit]

class SlackNotifierLive(
  slackClient: SlackClient,
  config: SlackConfig,
  discussionRepo: DiscussionRepository
) extends SlackNotifier:

  private val retryPolicy = Schedule.exponential(1.second) && Schedule.recurs(2)

  def notify(action: DiscussionActionConfirmed, broadcast: DiscussionActionConfirmed => Task[Unit]): Task[Unit] =
    action match
      case DiscussionActionConfirmed.AddResult(discussion) =>
        handleAdd(discussion, broadcast).fork.unit

      case DiscussionActionConfirmed.Rename(topicId, newTopic) =>
        handleRename(topicId, newTopic).fork.unit

      case DiscussionActionConfirmed.UpdateRoomSlot(topicId, roomSlot) =>
        handleUpdateRoomSlot(topicId, Some(roomSlot)).fork.unit

      case DiscussionActionConfirmed.Unschedule(topicId) =>
        handleUpdateRoomSlot(topicId, None).fork.unit

      case DiscussionActionConfirmed.Delete(topicId) =>
        handleDelete(topicId).fork.unit

      case DiscussionActionConfirmed.MoveTopic(topicId, targetRoomSlot) =>
        handleUpdateRoomSlot(topicId, Some(targetRoomSlot)).fork.unit

      case DiscussionActionConfirmed.SwapTopics(topic1, newSlot1, topic2, newSlot2) =>
        (handleUpdateRoomSlot(topic1, Some(newSlot1)) *> handleUpdateRoomSlot(topic2, Some(newSlot2))).fork.unit

      case _ => ZIO.unit // Vote, RemoveVote, SlackThreadLinked, Rejected are no-ops

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
      blocks = buildUpdateBlocks(row.copy(topic = newTopic.unwrap))
      _ <- slackClient.updateMessage(config.channelId, ts, blocks)
    yield ()

    effect.catchAll(err => ZIO.logError(s"Slack rename failed for topic $topicId: $err"))

  private def handleUpdateRoomSlot(topicId: TopicId, roomSlot: Option[RoomSlot]): Task[Unit] =
    val effect = for
      row <- discussionRepo.findById(topicId.unwrap).someOrFail(new Exception(s"Discussion $topicId not found"))
      ts  <- ZIO.fromOption(row.slackThreadTs).orElseFail(new Exception(s"No Slack thread for topic $topicId"))
      blocks = buildUpdateBlocks(row.copy(roomSlot = roomSlot.map(_.toJson)))
      _ <- slackClient.updateMessage(config.channelId, ts, blocks)
    yield ()

    effect.catchAll(err => ZIO.logError(s"Slack schedule update failed for topic $topicId: $err"))

  private def handleDelete(topicId: TopicId): Task[Unit] =
    val effect = for
      row <- discussionRepo.findById(topicId.unwrap).someOrFail(new Exception(s"Discussion $topicId not found"))
      ts  <- ZIO.fromOption(row.slackThreadTs).orElseFail(new Exception(s"No Slack thread for topic $topicId"))
      _   <- slackClient.postReply(config.channelId, ts, ":no_entry_sign: This topic has been cancelled.")
    yield ()

    effect.catchAll(err => ZIO.logError(s"Slack delete notification failed for topic $topicId: $err"))

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


class SlackNotifierNoOp extends SlackNotifier:
  def notify(action: DiscussionActionConfirmed, broadcast: DiscussionActionConfirmed => Task[Unit]): Task[Unit] =
    ZIO.unit

object SlackNotifier:
  val layer: ZLayer[Option[SlackConfig] & DiscussionRepository & Client, Nothing, SlackNotifier] =
    ZLayer.fromZIO:
      for
        maybeConfig    <- ZIO.service[Option[SlackConfig]]
        discussionRepo <- ZIO.service[DiscussionRepository]
        client         <- ZIO.service[Client]
        notifier <- maybeConfig match
          case Some(config) =>
            ZIO.logInfo(s"Slack integration enabled for channel ${config.channelId}") *>
              ZIO.succeed(SlackNotifierLive(SlackClientLive(client, config), config, discussionRepo))
          case None =>
            ZIO.logInfo("Slack integration disabled (SLACK_BOT_TOKEN, SLACK_CHANNEL_ID, or APP_BASE_URL not set)") *>
              ZIO.succeed(SlackNotifierNoOp())
      yield notifier
