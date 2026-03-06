package co.wtf.openspaces.slack

import co.wtf.openspaces.*
import co.wtf.openspaces.db.{DiscussionRepository, LightningTalkRepository, LightningTalkRow, HackathonProjectRepository, HackathonProjectRow, TopicVoteRepository, HackathonProjectMemberRepository, ActivityInterestRepository, SlackRosterMessageRepository, UserRepository}
import co.wtf.openspaces.discussions.{Discussion, DiscussionActionConfirmed, Feedback, VotePosition}
import co.wtf.openspaces.hackathon.*
import co.wtf.openspaces.lighting_talks.*
import co.wtf.openspaces.activities.*
import neotype.unwrap
import java.net.URLDecoder
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
  /** Fetch reply counts for all entities with Slack threads */
  def fetchReplyCounts: Task[SlackReplyCounts]
  /** Start background fiber that periodically fetches and broadcasts reply counts */
  def startReplyCountRefresh(
    broadcast: SlackReplyCountsMessage => Task[Unit],
    interval: Duration = 3.minutes,
  ): Task[Fiber.Runtime[Throwable, Unit]]

class SlackNotifierLive(
  slackClient: SlackClient,
  config: SlackConfig,
  discussionRepo: DiscussionRepository,
  lightningTalkRepo: LightningTalkRepository,
  hackathonProjectRepo: HackathonProjectRepository,
  activityRepo: co.wtf.openspaces.db.ActivityRepository,
  topicVoteRepo: TopicVoteRepository,
  hackathonMemberRepo: HackathonProjectMemberRepository,
  activityInterestRepo: ActivityInterestRepository,
  rosterService: SlackRosterService,
  userRepo: UserRepository,
  httpClient: Client,
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

      case DiscussionActionConfirmed.Vote(topicId, feedback) =>
        handleVoteRosterUpdate(topicId, feedback).fork.unit

      case _ => ZIO.unit // SlackThreadLinked, Rejected are no-ops

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
      case HackathonProjectActionConfirmed.Joined(projectId, _, _) =>
        handleHackathonRosterUpdate(projectId).fork.unit
      case HackathonProjectActionConfirmed.Left(projectId, _, _) =>
        handleHackathonRosterUpdate(projectId).fork.unit
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
      case ActivityActionConfirmed.InterestSet(activityId, _, _, _, _) =>
        handleActivityRosterUpdate(activityId).fork.unit
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
      // Also clean up the roster message
      _   <- rosterService.deleteRoster("discussion", topicId.unwrap)
    yield ()

    effect.catchAll(err => ZIO.logError(s"Slack delete failed for topic $topicId: $err"))

  private def handleVoteRosterUpdate(topicId: TopicId, feedback: Feedback): Task[Unit] =
    val effect = for
      row <- discussionRepo.findById(topicId.unwrap).someOrFail(new Exception(s"Discussion $topicId not found"))
      channelId <- ZIO.fromOption(row.slackChannelId).orElseFail(new Exception(s"No Slack channel for topic $topicId"))
      threadTs <- ZIO.fromOption(row.slackThreadTs).orElseFail(new Exception(s"No Slack thread for topic $topicId"))
      // Get all current votes for this topic
      allVotes <- topicVoteRepo.findAllForActiveDiscussions
      topicVotes = allVotes.filter(v => v.topicId == topicId.unwrap && v.position == "Interested")
      interestedUsers = topicVotes.map(v => Person(v.githubUsername)).toList
      // Update the roster message
      _ <- rosterService.updateRoster("discussion", topicId.unwrap, channelId, threadTs, interestedUsers)
      // If user voted "Interested" and has a Slack access token, post wave emoji as them to auto-follow thread
      _ <- ZIO.when(feedback.position == VotePosition.Interested)(
        postWaveAsUser(feedback.voter.unwrap, channelId, threadTs)
      )
    yield ()

    effect.catchAll(err => ZIO.logWarning(s"Roster update failed for topic $topicId: ${err.getMessage}"))

  /** Add a wave reaction as the user to make them auto-follow the thread */
  private def postWaveAsUser(githubUsername: String, channelId: String, threadTs: String): Task[Unit] =
    val effect = for
      accessTokenOpt <- userRepo.findSlackAccessToken(githubUsername)
      _ <- accessTokenOpt match
        case Some(accessToken) =>
          addReactionAsUser(accessToken, channelId, threadTs, "wave")
        case None =>
          ZIO.logDebug(s"User $githubUsername has no Slack access token, skipping wave reaction")
    yield ()
    effect.catchAll(err => ZIO.logWarning(s"Failed to add wave reaction as $githubUsername: ${err.getMessage}"))

  /** Add a reaction to a message using a user's access token */
  private def addReactionAsUser(accessToken: String, channelId: String, messageTs: String, emoji: String): Task[Unit] =
    import zio.http.*
    ZIO.scoped {
      for
        url <- ZIO.fromEither(URL.decode("https://slack.com/api/reactions.add"))
          .mapError(e => new Exception(s"Invalid Slack URL: $e"))
        payload = s"""{"channel":"$channelId","timestamp":"$messageTs","name":"$emoji"}"""
        response <- httpClient
          .addHeader(Header.Authorization.Bearer(accessToken))
          .addHeader(Header.ContentType(MediaType.application.json))
          .url(url)
          .post("")(Body.fromString(payload))
        body <- response.body.asString
        json <- ZIO.fromEither(body.fromJson[zio.json.ast.Json])
          .mapError(e => new Exception(s"Invalid JSON from Slack: $e"))
        ok = json match
          case zio.json.ast.Json.Obj(fields) => fields.collectFirst { case ("ok", zio.json.ast.Json.Bool(v)) => v }.getOrElse(false)
          case _ => false
        // "already_reacted" is fine - user already has this reaction
        error = json match
          case zio.json.ast.Json.Obj(fields) => fields.collectFirst { case ("error", zio.json.ast.Json.Str(e)) => e }
          case _ => None
        _ <- ZIO.when(!ok && error != Some("already_reacted"))(ZIO.logWarning(s"Slack reactions.add failed: $body"))
      yield ()
    }

  private def handleHackathonRosterUpdate(projectId: HackathonProjectId): Task[Unit] =
    val effect = for
      row <- hackathonProjectRepo.findById(projectId.unwrap).someOrFail(new Exception(s"Project $projectId not found"))
      channelId <- ZIO.fromOption(row.slackChannelId).orElseFail(new Exception(s"No Slack channel for project $projectId"))
      threadTs <- ZIO.fromOption(row.slackThreadTs).orElseFail(new Exception(s"No Slack thread for project $projectId"))
      // Get all current members for this project
      members <- hackathonMemberRepo.findActiveByProject(projectId.unwrap)
      memberUsers = members.map(m => Person(m.githubUsername)).toList
      // Update the roster message
      _ <- rosterService.updateRoster("hackathon_project", projectId.unwrap, channelId, threadTs, memberUsers)
    yield ()

    effect.catchAll(err => ZIO.logWarning(s"Roster update failed for hackathon project $projectId: ${err.getMessage}"))

  private def handleActivityRosterUpdate(activityId: ActivityId): Task[Unit] =
    val effect = for
      row <- activityRepo.findById(activityId.unwrap).someOrFail(new Exception(s"Activity $activityId not found"))
      channelId <- ZIO.fromOption(row.slackChannelId).orElseFail(new Exception(s"No Slack channel for activity $activityId"))
      threadTs <- ZIO.fromOption(row.slackThreadTs).orElseFail(new Exception(s"No Slack thread for activity $activityId"))
      // Get all current interested users for this activity
      interested <- activityInterestRepo.findByActivity(activityId.unwrap)
      interestedUsers = interested.map(i => Person(i.githubUsername)).toList
      // Update the roster message
      _ <- rosterService.updateRoster("activity", activityId.unwrap, channelId, threadTs, interestedUsers)
    yield ()

    effect.catchAll(err => ZIO.logWarning(s"Roster update failed for activity $activityId: ${err.getMessage}"))

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
      // Also clean up the roster message
      _ <- rosterService.deleteRoster("hackathon_project", projectId.unwrap)
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

  // Track whether we've logged the missing scope warning (to avoid log spam)
  private val hasLoggedMissingScope: Ref[Boolean] = Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(Ref.make(false)).getOrThrow()
  }

  private case class ReplyCountDiagnostics(
    success: Int = 0,
    missingScope: Int = 0,
    notFound: Int = 0,
    rateLimited: Int = 0,
    errors: Int = 0,
  )

  def fetchReplyCounts: Task[SlackReplyCounts] =
    for
      discussions <- discussionRepo.findAllActive
      lightningTalks <- lightningTalkRepo.findAll
      hackathonProjects <- hackathonProjectRepo.findAllActive
      activities <- activityRepo.findAllActive
      
      // Fetch counts for each entity type, using stored channel/ts when available and
      // falling back to parsing them from the Slack permalink for older rows.
      discussionCounts <- fetchCountsForEntities(
        "discussions",
        discussions.flatMap(d => slackThreadRef(d.slackChannelId, d.slackThreadTs, d.slackPermalink).map((d.id, _, _)))
      )
      lightningCounts <- fetchCountsForEntities(
        "lightning talks",
        lightningTalks.flatMap(lt => slackThreadRef(lt.slackChannelId, lt.slackThreadTs, lt.slackPermalink).map((lt.id, _, _)))
      )
      hackathonCounts <- fetchCountsForEntities(
        "hackathon projects",
        hackathonProjects.flatMap(hp => slackThreadRef(hp.slackChannelId, hp.slackThreadTs, hp.slackPermalink).map((hp.id, _, _)))
      )
      activityCounts <- fetchCountsForEntities(
        "activities",
        activities.flatMap(a => slackThreadRef(a.slackChannelId, a.slackThreadTs, a.slackPermalink).map((a.id, _, _)))
      )
    yield SlackReplyCounts(
      discussions = discussionCounts,
      lightningTalks = lightningCounts,
      hackathonProjects = hackathonCounts,
      activities = activityCounts,
    )

  private def slackThreadRef(
    channelId: Option[String],
    threadTs: Option[String],
    permalink: Option[String],
  ): Option[(String, String)] =
    channelId.zip(threadTs).orElse(
      permalink.flatMap(extractSlackThreadRefFromPermalink),
    )

  private def extractSlackThreadRefFromPermalink(permalink: String): Option[(String, String)] =
    val query = permalink.split("\\?", 2).lift(1)
    query.flatMap { q =>
      val params =
        q.split("&").flatMap { pair =>
          pair.split("=", 2) match
            case Array(key, value) =>
              Some(key -> URLDecoder.decode(value, "UTF-8"))
            case _ =>
              None
        }.toMap
      params.get("cid").zip(params.get("thread_ts"))
    }

  private def fetchCountsForEntities(
    entityType: String,
    entities: Vector[(Long, String, String)] // (id, channelId, threadTs)
  ): Task[Map[String, Int]] =
    // Process sequentially with delay to avoid Slack Web API rate limits.
    // conversations.replies is Tier 3 (50+/min) but non-Marketplace apps face stricter limits.
    // 3s delay = 20 req/min, safely within Tier 2 limits and tolerant of stricter enforcement.
    // Keys are String for JSON compatibility (JS object keys are always strings)
    ZIO.foldLeft(entities)((Map.empty[String, Int], ReplyCountDiagnostics())) { case ((counts, diagnostics), (id, channel, threadTs)) =>
      for
        _ <- ZIO.sleep(3000.millis)
        result <- slackClient.getReplyCount(channel, threadTs).flatMap {
          case ReplyCountResult.Count(n) =>
            ZIO.succeed(
              (
                counts.updated(id.toString, n),
                diagnostics.copy(success = diagnostics.success + 1),
              ),
            )
          case ReplyCountResult.MissingScope =>
            // Log once, then suppress
            hasLoggedMissingScope.get.flatMap { alreadyLogged =>
              if !alreadyLogged then
                ZIO.logWarning("Slack channels:history scope not granted - reply counts unavailable") *>
                hasLoggedMissingScope.set(true) *>
                ZIO.succeed(
                  (
                    counts,
                    diagnostics.copy(missingScope = diagnostics.missingScope + 1),
                  ),
                )
              else
                ZIO.succeed(
                  (
                    counts,
                    diagnostics.copy(missingScope = diagnostics.missingScope + 1),
                  ),
                )
            }
          case ReplyCountResult.NotFound =>
            ZIO.succeed(
              (
                counts,
                diagnostics.copy(notFound = diagnostics.notFound + 1),
              ),
            )
          case ReplyCountResult.Error("ratelimited") =>
            ZIO.logWarning(
              s"Slack rate limited while fetching $entityType reply count for id=$id channel=$channel thread=$threadTs"
            ) *>
            ZIO.succeed(
              (
                counts,
                diagnostics.copy(rateLimited = diagnostics.rateLimited + 1),
              ),
            )
          case ReplyCountResult.Error(msg) =>
            ZIO.logWarning(
              s"Failed to fetch $entityType reply count for id=$id channel=$channel thread=$threadTs: $msg"
            ) *>
            ZIO.succeed(
              (
                counts,
                diagnostics.copy(errors = diagnostics.errors + 1),
              ),
            )
        }
      yield result
    }.flatMap { case (counts, diagnostics) =>
      ZIO.logInfo(
        s"Slack reply count fetch for $entityType: tracked=${entities.size}, successes=${diagnostics.success}, missingScope=${diagnostics.missingScope}, notFound=${diagnostics.notFound}, rateLimited=${diagnostics.rateLimited}, errors=${diagnostics.errors}"
      ).as(counts)
    }

  def startReplyCountRefresh(
    broadcast: SlackReplyCountsMessage => Task[Unit],
    interval: Duration = 3.minutes,
  ): Task[Fiber.Runtime[Throwable, Unit]] =
    val refreshLoop = (for
      startedAt <- Clock.nanoTime
      _ <- ZIO.logInfo(s"Fetching Slack reply counts...")
      counts <- fetchReplyCounts
      total = counts.discussions.size + counts.lightningTalks.size + 
              counts.hackathonProjects.size + counts.activities.size
      totalReplies = counts.discussions.values.sum +
                     counts.lightningTalks.values.sum +
                     counts.hackathonProjects.values.sum +
                     counts.activities.values.sum
      _ <- ZIO.when(total > 0)(
        ZIO.logInfo(s"Broadcasting reply counts for $total threads (total replies: $totalReplies)") *>
        broadcast(SlackReplyCountsMessage(counts))
      )
      finishedAt <- Clock.nanoTime
      elapsedSeconds = (finishedAt - startedAt) / 1000000000L
      _ <- ZIO.logInfo(s"Slack reply count refresh cycle completed in ${elapsedSeconds}s")
    yield ()).catchAll { error =>
      ZIO.logError(s"Error fetching Slack reply counts: ${error.getMessage}")
    }
    
    // Initial fetch, then repeat on interval
    (refreshLoop *> refreshLoop.schedule(Schedule.fixed(interval)).unit).fork


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
  def fetchReplyCounts: Task[SlackReplyCounts] =
    ZIO.succeed(SlackReplyCounts(Map.empty, Map.empty, Map.empty, Map.empty))
  def startReplyCountRefresh(
    broadcast: SlackReplyCountsMessage => Task[Unit],
    interval: Duration = 3.minutes,
  ): Task[Fiber.Runtime[Throwable, Unit]] =
    // No-op: just return a fiber that does nothing
    ZIO.never.fork

object SlackNotifier:
  val layer: ZLayer[Option[SlackConfigEnv] & DiscussionRepository & LightningTalkRepository & HackathonProjectRepository & co.wtf.openspaces.db.ActivityRepository & TopicVoteRepository & HackathonProjectMemberRepository & ActivityInterestRepository & SlackRosterMessageRepository & UserRepository & Client, Nothing, SlackNotifier] =
    ZLayer.fromZIO:
      for
        maybeEnvConfig <- ZIO.service[Option[SlackConfigEnv]]
        discussionRepo <- ZIO.service[DiscussionRepository]
        lightningTalkRepo <- ZIO.service[LightningTalkRepository]
        hackathonProjectRepo <- ZIO.service[HackathonProjectRepository]
        activityRepo <- ZIO.service[co.wtf.openspaces.db.ActivityRepository]
        topicVoteRepo <- ZIO.service[TopicVoteRepository]
        hackathonMemberRepo <- ZIO.service[HackathonProjectMemberRepository]
        activityInterestRepo <- ZIO.service[ActivityInterestRepository]
        rosterMessageRepo <- ZIO.service[SlackRosterMessageRepository]
        userRepo <- ZIO.service[UserRepository]
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
              slackClient = SlackClientLive(client, config)
              rosterService = SlackRosterServiceLive(slackClient, rosterMessageRepo, userRepo)
              _ <- ZIO.logInfo(
                s"Slack integration enabled for channels #${envConfig.channelName} ($channelId), #${envConfig.lightningChannelName} ($lightningChannelId), #${envConfig.activityChannelName} ($activityChannelId), #${envConfig.hackathonChannelName} ($hackathonChannelId), #${envConfig.accessRequestChannelName} ($accessRequestChannelId)"
              )
            yield SlackNotifierLive(
              slackClient,
              config,
              discussionRepo,
              lightningTalkRepo,
              hackathonProjectRepo,
              activityRepo,
              topicVoteRepo,
              hackathonMemberRepo,
              activityInterestRepo,
              rosterService,
              userRepo,
              client,
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
