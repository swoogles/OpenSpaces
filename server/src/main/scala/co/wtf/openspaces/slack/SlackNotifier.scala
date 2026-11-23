package co.wtf.openspaces.slack

import co.wtf.openspaces.*
import co.wtf.openspaces.WebSocketMessageFromServer.*
import co.wtf.openspaces.db.{ActivityRow, DiscussionRepository, DiscussionRow, LightningTalkRepository, HackathonProjectRepository, HackathonProjectRow, TopicVoteRepository, HackathonProjectMemberRepository, ActivityInterestRepository, SlackRosterMessageRepository, UserRepository}
import co.wtf.openspaces.discussions.{Discussion, DiscussionActionConfirmed, Feedback, VotePosition}
import co.wtf.openspaces.hackathon.*
import co.wtf.openspaces.lighting_talks.*
import co.wtf.openspaces.activities.*
import neotype.unwrap
import java.net.URLDecoder
import zio.*
import zio.http.Client
import zio.json.*
import zio.json.ast.Json

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

      case DiscussionActionConfirmed.Delete(topicId) =>
        handleDelete(topicId).fork.unit

      case DiscussionActionConfirmed.Vote(topicId, feedback) =>
        handleVoteRosterUpdate(topicId, feedback).fork.unit

      case _ => ZIO.unit // Scheduling moves, SlackThreadLinked, Rejected are no-ops

  def notifyLightning(
    action: LightningTalkActionConfirmed,
    broadcast: LightningTalkActionConfirmed => Task[Unit],
  ): Task[Unit] =
    action match
      case LightningTalkActionConfirmed.AddResult(proposal) =>
        handleLightningAdd(proposal, broadcast).fork.unit
      case LightningTalkActionConfirmed.Delete(proposalId, slackChannelId, slackThreadTs) =>
        deleteThread(s"lightning talk $proposalId", slackChannelId, slackThreadTs, config.lightningChannelId).fork.unit
      case LightningTalkActionConfirmed.SetAssignment(proposalId, _) =>
        handleLightningAssignmentUpdate(proposalId).fork.unit
      case LightningTalkActionConfirmed.DrawForNightResult(_, assignments) =>
        ZIO.foreachDiscard(assignments)(a => handleLightningAssignmentUpdate(a.proposalId)).fork.unit
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
        deleteThread(s"activity $activityId", slackChannelId, slackThreadTs, config.activityChannelId).fork.unit
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
      _ <- slackClient
        .postMessage(config.accessRequestChannelId, SlackBlocks.accessRequest(username, displayName, config.appBaseUrl))
        .retry(retryPolicy)
    yield ()

    effect
      .catchAll(err => ZIO.logError(s"Failed to send access request notification for $username: $err"))
      .fork
      .unit

  // ---------------------------------------------------------------------------
  // Shared Slack thread plumbing

  /** Where an entity's Slack thread lives. */
  private case class SlackThread(channelId: String, ts: String)

  /** Post the entity's card, store the thread on its row, then tell clients the permalink. */
  private def postAndLink(
    label: String,
    channelId: String,
    blocks: Json,
    saveThread: (String, String, String) => Task[Unit],
    linked: String => Task[Unit],
  ): Task[Unit] =
    val effect = for
      ref       <- slackClient.postMessage(channelId, blocks).retry(retryPolicy)
      permalink <- slackClient.getPermalink(channelId, ref.ts).retry(retryPolicy)
      _         <- saveThread(channelId, ref.ts, permalink)
      _         <- linked(permalink)
    yield ()
    effect.catchAll(err => ZIO.logError(s"Slack post failed for $label: $err"))

  /** Load an entity's row and run `f` against its Slack thread. Failures are logged, never propagated. */
  private def withThread[R](
    label: String,
    findRow: Task[Option[R]],
    channelId: R => Option[String],
    threadTs: R => Option[String],
    defaultChannel: String,
  )(f: (R, SlackThread) => Task[Unit]): Task[Unit] =
    val effect = for
      row <- findRow.someOrFail(new Exception(s"$label not found"))
      ts  <- ZIO.fromOption(threadTs(row)).orElseFail(new Exception(s"No Slack thread for $label"))
      // Use the channel where the thread was created, not the current config channel
      _   <- f(row, SlackThread(channelId(row).getOrElse(defaultChannel), ts))
    yield ()
    effect.catchAll(err => ZIO.logWarning(s"Slack update failed for $label: ${err.getMessage}"))

  /** Delete a thread whose coordinates were captured before its row was removed. */
  private def deleteThread(
    label: String,
    channelId: Option[String],
    threadTs: Option[String],
    defaultChannel: String,
  ): Task[Unit] =
    ZIO.foreachDiscard(threadTs)(ts =>
      slackClient
        .deleteMessage(channelId.getOrElse(defaultChannel), ts)
        .catchAll(err => ZIO.logError(s"Slack delete failed for $label: $err")),
    )

  // ---------------------------------------------------------------------------
  // Discussions

  private def discussionThread(topicId: TopicId)(f: (DiscussionRow, SlackThread) => Task[Unit]): Task[Unit] =
    withThread(s"topic $topicId", discussionRepo.findById(topicId.unwrap), _.slackChannelId, _.slackThreadTs, config.channelId)(f)

  private def handleAdd(discussion: Discussion, broadcast: DiscussionActionConfirmed => Task[Unit]): Task[Unit] =
    postAndLink(
      s"topic ${discussion.id}",
      config.channelId,
      SlackBlocks.discussion(
        discussion.topicName,
        discussion.facilitator.unwrap,
        discussion.facilitatorName,
        discussion.roomSlot,
        config.appBaseUrl,
      ),
      discussionRepo.updateSlackThread(discussion.id.unwrap, _, _, _),
      permalink => broadcast(DiscussionActionConfirmed.SlackThreadLinked(discussion.id, permalink)),
    )

  private def handleRename(topicId: TopicId, newTopic: Topic): Task[Unit] =
    discussionThread(topicId) { (row, thread) =>
      slackClient.updateMessage(
        thread.channelId,
        thread.ts,
        SlackBlocks.discussion(newTopic.unwrap, row.facilitator, row.facilitator, None, config.appBaseUrl),
      )
    }

  private def handleDelete(topicId: TopicId): Task[Unit] =
    discussionThread(topicId) { (_, thread) =>
      slackClient.deleteMessage(thread.channelId, thread.ts) *>
        rosterService.deleteRoster("discussion", topicId.unwrap)
    }

  private def handleVoteRosterUpdate(topicId: TopicId, feedback: Feedback): Task[Unit] =
    discussionThread(topicId) { (_, thread) =>
      for
        allVotes <- topicVoteRepo.findAllForActiveDiscussions
        interestedUsers = allVotes
          .filter(v => v.topicId == topicId.unwrap && v.position == VotePosition.Interested.toString)
          .map(v => Person(v.githubUsername))
          .toList
        _ <- rosterService.updateRoster("discussion", topicId.unwrap, thread.channelId, thread.ts, interestedUsers)
        // Voting "Interested" with a linked Slack account auto-follows the thread via a wave reaction
        _ <- ZIO.when(feedback.position == VotePosition.Interested)(
          postWaveAsUser(feedback.voter.unwrap, thread.channelId, thread.ts),
        )
      yield ()
    }

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
        payload = Json.Obj(
          "channel"   -> Json.Str(channelId),
          "timestamp" -> Json.Str(messageTs),
          "name"      -> Json.Str(emoji),
        ).toJson
        response <- httpClient
          .addHeader(Header.Authorization.Bearer(accessToken))
          .addHeader(Header.ContentType(MediaType.application.json))
          .url(url)
          .post("")(Body.fromString(payload))
        body <- response.body.asString
        json <- ZIO.fromEither(body.fromJson[Json])
          .mapError(e => new Exception(s"Invalid JSON from Slack: $e"))
        fields = json match
          case Json.Obj(fs) => fs.toMap
          case _            => Map.empty[String, Json]
        ok = fields.get("ok").contains(Json.Bool(true))
        // "already_reacted" is fine - user already has this reaction
        alreadyReacted = fields.get("error").contains(Json.Str("already_reacted"))
        _ <- ZIO.when(!ok && !alreadyReacted)(ZIO.logWarning(s"Slack reactions.add failed: $body"))
      yield ()
    }

  // ---------------------------------------------------------------------------
  // Lightning talks

  private def lightningBlocks(speakerLogin: String, night: Option[String], slot: Option[Int]): Json =
    SlackBlocks.lightningTalk(speakerLogin, night.zip(slot), config.appBaseUrl)

  private def handleLightningAdd(
    proposal: LightningTalkProposal,
    broadcast: LightningTalkActionConfirmed => Task[Unit],
  ): Task[Unit] =
    postAndLink(
      s"lightning proposal ${proposal.id}",
      config.lightningChannelId,
      lightningBlocks(proposal.speaker.unwrap, None, None),
      lightningTalkRepo.updateSlackThread(proposal.id.unwrap, _, _, _),
      permalink => broadcast(LightningTalkActionConfirmed.SlackThreadLinked(proposal.id, permalink)),
    )

  private def handleLightningAssignmentUpdate(proposalId: LightningTalkId): Task[Unit] =
    withThread(
      s"lightning talk $proposalId",
      lightningTalkRepo.findById(proposalId.unwrap),
      _.slackChannelId,
      _.slackThreadTs,
      config.lightningChannelId,
    ) { (row, thread) =>
      slackClient.updateMessage(thread.channelId, thread.ts, lightningBlocks(row.speaker, row.assignmentNight, row.assignmentSlot))
    }

  // ---------------------------------------------------------------------------
  // Hackathon projects

  private def projectThread(projectId: HackathonProjectId)(f: (HackathonProjectRow, SlackThread) => Task[Unit]): Task[Unit] =
    withThread(
      s"hackathon project $projectId",
      hackathonProjectRepo.findById(projectId.unwrap),
      _.slackChannelId,
      _.slackThreadTs,
      config.hackathonChannelId,
    )(f)

  private def handleHackathonCreate(
    project: HackathonProject,
    broadcast: HackathonProjectActionConfirmed => Task[Unit],
  ): Task[Unit] =
    postAndLink(
      s"hackathon project ${project.id}",
      config.hackathonChannelId,
      SlackBlocks.hackathonProject(project.titleText, project.owner.unwrap, project.ownerName, config.appBaseUrl),
      hackathonProjectRepo.updateSlackThread(project.id.unwrap, _, _, _),
      permalink => broadcast(HackathonProjectActionConfirmed.SlackThreadLinked(project.id, permalink)),
    )

  private def handleHackathonRosterUpdate(projectId: HackathonProjectId): Task[Unit] =
    projectThread(projectId) { (_, thread) =>
      for
        members <- hackathonMemberRepo.findActiveByProject(projectId.unwrap)
        _ <- rosterService.updateRoster(
          "hackathon_project",
          projectId.unwrap,
          thread.channelId,
          thread.ts,
          members.map(m => Person(m.githubUsername)).toList,
        )
      yield ()
    }

  private def handleHackathonRename(projectId: HackathonProjectId, newTitle: ProjectTitle): Task[Unit] =
    projectThread(projectId) { (row, thread) =>
      slackClient.updateMessage(
        thread.channelId,
        thread.ts,
        SlackBlocks.hackathonProject(newTitle.unwrap, row.owner, row.owner, config.appBaseUrl),
      )
    }

  private def handleHackathonDelete(projectId: HackathonProjectId): Task[Unit] =
    projectThread(projectId) { (_, thread) =>
      slackClient.deleteMessage(thread.channelId, thread.ts) *>
        rosterService.deleteRoster("hackathon_project", projectId.unwrap)
    }

  // ---------------------------------------------------------------------------
  // Activities

  private def activityThread(activityId: ActivityId)(f: (ActivityRow, SlackThread) => Task[Unit]): Task[Unit] =
    withThread(
      s"activity $activityId",
      activityRepo.findById(activityId.unwrap),
      _.slackChannelId,
      _.slackThreadTs,
      config.activityChannelId,
    )(f)

  private def handleActivityCreate(
    activity: Activity,
    broadcast: ActivityActionConfirmed => Task[Unit],
  ): Task[Unit] =
    postAndLink(
      s"activity ${activity.id}",
      config.activityChannelId,
      SlackBlocks.activity(
        activity.descriptionText,
        activity.creator.unwrap,
        activity.creatorName,
        Some(activity.eventTime),
        config.appBaseUrl,
      ),
      activityRepo.updateSlackThread(activity.id.unwrap, _, _, _),
      permalink => broadcast(ActivityActionConfirmed.SlackThreadLinked(activity.id, permalink)),
    )

  private def handleActivityUpdate(
    activityId: ActivityId,
    newDescription: ActivityDescription,
    newEventTime: java.time.LocalDateTime,
  ): Task[Unit] =
    activityThread(activityId) { (row, thread) =>
      slackClient.updateMessage(
        thread.channelId,
        thread.ts,
        SlackBlocks.activity(newDescription.unwrap, row.creator, row.creator, Some(newEventTime), config.appBaseUrl),
      )
    }

  private def handleActivityRosterUpdate(activityId: ActivityId): Task[Unit] =
    activityThread(activityId) { (_, thread) =>
      for
        interested <- activityInterestRepo.findByActivity(activityId.unwrap)
        _ <- rosterService.updateRoster(
          "activity",
          activityId.unwrap,
          thread.channelId,
          thread.ts,
          interested.map(i => Person(i.githubUsername)).toList,
        )
      yield ()
    }

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
