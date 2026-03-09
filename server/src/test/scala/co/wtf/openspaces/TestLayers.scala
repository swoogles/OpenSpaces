package co.wtf.openspaces

import co.wtf.openspaces.auth.{AdminConfig, AuthenticatedTicketService}
import co.wtf.openspaces.discussions.{Discussion, DiscussionAction, DiscussionActionConfirmed, DiscussionDataStore, DiscussionState, DiscussionStore, SchedulingService}
import co.wtf.openspaces.github.GitHubProfileService
import co.wtf.openspaces.hackathon.*
import co.wtf.openspaces.lightning_talks.LightningTalkService
import co.wtf.openspaces.lighting_talks.*
import co.wtf.openspaces.activities.*
import neotype.unwrap
import zio.*
import zio.direct.*
import zio.http.*
import zio.http.ChannelEvent.UserEvent
import zio.json.*
import zio.schema.codec.JsonCodec.zioJsonBinaryCodec
import zio.test.*

/** Test-only layers for services that need database or external dependencies */
object TestLayers:

  /** SchedulingService layer (uses real implementation since it has no DB deps) */
  val schedulingServiceLayer: ZLayer[SessionService & DiscussionStore, Nothing, SchedulingService] =
    SchedulingService.layer

  /** No-op SlackNotifier for tests */
  val slackNotifierLayer: ULayer[co.wtf.openspaces.slack.SlackNotifier] =
    ZLayer.succeed(co.wtf.openspaces.slack.SlackNotifierNoOp())

  /** In-memory LightningTalkService for tests */
  val lightningTalkServiceLayer: ULayer[LightningTalkService] =
    ZLayer.fromZIO:
      for
        stateRef <- Ref.make(LightningTalkState(Map.empty))
      yield LightningTalkService(
        stateRef,
        NoOpLightningTalkRepository,
        NoOpUserRepository,
        NoOpGitHubProfileService,
      )

  /** In-memory HackathonProjectService for tests */
  val hackathonProjectServiceLayer: ULayer[HackathonProjectService] =
    ZLayer.fromZIO:
      for
        stateRef <- Ref.make(HackathonProjectState(Map.empty))
      yield HackathonProjectService(
        stateRef,
        NoOpHackathonProjectRepository,
        NoOpHackathonProjectMemberRepository,
        NoOpUserRepository,
        NoOpGitHubProfileService,
      )

  /** In-memory ActivityService for tests */
  val activityServiceLayer: ULayer[ActivityService] =
    ZLayer.fromZIO:
      for
        stateRef <- Ref.make(ActivityState(Map.empty))
      yield ActivityService(
        stateRef,
        NoOpActivityRepository,
        NoOpActivityInterestRepository,
        NoOpActivityDismissalRepository,
        NoOpTimeSlotRepository,
        NoOpUserRepository,
        NoOpGitHubProfileService,
      )

  /** In-memory LocationService for tests */
  val locationServiceLayer: ULayer[co.wtf.openspaces.location.LocationService] =
    ZLayer.fromZIO:
      for
        stateRef <- Ref.make(co.wtf.openspaces.location.LocationState.empty)
      yield co.wtf.openspaces.location.LocationService(stateRef, NoOpUserRepository)

  /** No-op DiscussionRepository for tests */
  val discussionRepositoryLayer: ULayer[co.wtf.openspaces.db.DiscussionRepository] =
    ZLayer.succeed(NoOpDiscussionRepository)

  /** No-op ConfirmedActionRepository for tests */
  val confirmedActionRepositoryLayer
      : ULayer[co.wtf.openspaces.db.ConfirmedActionRepository] =
    ZLayer.succeed(NoOpConfirmedActionRepository)

  /** No-op UserRepository for tests */
  val userRepositoryLayer: ULayer[co.wtf.openspaces.db.UserRepository] =
    ZLayer.succeed(NoOpUserRepository)

  // No-op repository implementations

  private object NoOpLightningTalkRepository
      extends co.wtf.openspaces.db.LightningTalkRepository:
    def findById(
      id: Long,
    ): Task[Option[co.wtf.openspaces.db.LightningTalkRow]] = ZIO.none
    def findBySpeaker(
      speaker: String,
    ): Task[Option[co.wtf.openspaces.db.LightningTalkRow]] = ZIO.none
    def findAll: Task[Vector[co.wtf.openspaces.db.LightningTalkRow]] =
      ZIO.succeed(Vector.empty)
    def insert(row: co.wtf.openspaces.db.LightningTalkRow): Task[Unit] =
      ZIO.unit
    def update(row: co.wtf.openspaces.db.LightningTalkRow): Task[Unit] =
      ZIO.unit
    def delete(id: Long): Task[Unit] = ZIO.unit
    def updateSlackThread(
      proposalId: Long,
      channelId: String,
      threadTs: String,
      permalink: String,
    ): Task[Unit] = ZIO.unit

  private object NoOpUserRepository extends co.wtf.openspaces.db.UserRepository:
    private def userApproved(username: String): Boolean =
      !username.toLowerCase.startsWith("pending")

    def findByUsername(
      username: String,
    ): Task[Option[co.wtf.openspaces.db.UserRow]] =
      ZIO.succeed(Some(
        co.wtf.openspaces.db.UserRow(
          githubUsername = username,
          displayName = Some(username),
          createdAt = java.time.OffsetDateTime.now(),
          approved = userApproved(username),
          slackUserId = None,
          slackAccessToken = None,
        ),
      ))
    def upsert(
      username: String,
      displayName: Option[String],
    ): Task[co.wtf.openspaces.db.UpsertResult] =
      ZIO.succeed(
        co.wtf.openspaces.db.UpsertResult(
          user = co.wtf.openspaces.db.UserRow(
            githubUsername = username,
            displayName = displayName,
            createdAt = java.time.OffsetDateTime.now(),
            approved = userApproved(username),
            slackUserId = None,
            slackAccessToken = None,
          ),
          isNewUser = false,
        ),
      )
    def findAll: Task[Vector[co.wtf.openspaces.db.UserRow]] =
      ZIO.succeed(Vector.empty)
    def deleteByUsernames(usernames: List[String]): Task[Int] =
      ZIO.succeed(0)
    def findPendingUsers: Task[Vector[co.wtf.openspaces.db.UserRow]] =
      ZIO.succeed(Vector.empty)
    def findApprovedUsers: Task[Vector[co.wtf.openspaces.db.UserRow]] =
      ZIO.succeed(Vector.empty)
    def approveUser(username: String): Task[Boolean] =
      ZIO.succeed(true)
    def revokeUser(username: String): Task[Boolean] =
      ZIO.succeed(true)
    def isApproved(username: String): Task[Boolean] =
      ZIO.succeed(userApproved(username))
    def linkSlackUserId(githubUsername: String, slackUserId: String): Task[Unit] =
      ZIO.unit
    def unlinkSlackUserId(githubUsername: String): Task[Unit] =
      ZIO.unit
    def linkSlackAccount(githubUsername: String, slackUserId: String, accessToken: String): Task[Unit] =
      ZIO.unit
    def findSlackAccessToken(githubUsername: String): Task[Option[String]] =
      ZIO.none
    def findUsersWithSlackIds(githubUsernames: List[String]): Task[Map[String, String]] =
      ZIO.succeed(Map.empty)

  private object NoOpGitHubProfileService extends GitHubProfileService:
    def ensureUserWithDisplayName(
      username: String,
    ): Task[co.wtf.openspaces.db.UserRow] =
      ZIO.succeed(
        co.wtf.openspaces.db.UserRow(
          githubUsername = username,
          displayName = Some(username),
          createdAt = java.time.OffsetDateTime.now(),
          approved = true,
          slackUserId = None,
          slackAccessToken = None,
        ),
      )

  private object NoOpActivityRepository extends co.wtf.openspaces.db.ActivityRepository:
    def findById(id: Long): Task[Option[co.wtf.openspaces.db.ActivityRow]] = ZIO.none
    def findAllActive: Task[Vector[co.wtf.openspaces.db.ActivityRow]] = ZIO.succeed(Vector.empty)
    def insert(row: co.wtf.openspaces.db.ActivityRow): Task[Unit] = ZIO.unit
    def update(row: co.wtf.openspaces.db.ActivityRow): Task[Unit] = ZIO.unit
    def updateOwner(id: Long, newOwner: String): Task[Unit] = ZIO.unit
    def softDelete(id: Long): Task[Unit] = ZIO.unit
    def updateSlackThread(id: Long, channelId: String, threadTs: String, permalink: String): Task[Unit] = ZIO.unit

  private object NoOpActivityInterestRepository extends co.wtf.openspaces.db.ActivityInterestRepository:
    def findByActivity(activityId: Long): Task[Vector[co.wtf.openspaces.db.ActivityInterestRow]] = ZIO.succeed(Vector.empty)
    def addInterest(activityId: Long, username: String): Task[co.wtf.openspaces.db.ActivityInterestRow] =
      ZIO.succeed(
        co.wtf.openspaces.db.ActivityInterestRow(
          activityId,
          username,
          java.time.OffsetDateTime.now(),
        ),
      )
    def removeInterest(activityId: Long, username: String): Task[Unit] = ZIO.unit

  private object NoOpActivityDismissalRepository extends co.wtf.openspaces.db.ActivityDismissalRepository:
    def findByActivity(activityId: Long): Task[Vector[co.wtf.openspaces.db.ActivityDismissalRow]] = ZIO.succeed(Vector.empty)
    def addDismissal(activityId: Long, username: String): Task[co.wtf.openspaces.db.ActivityDismissalRow] =
      ZIO.succeed(
        co.wtf.openspaces.db.ActivityDismissalRow(
          activityId,
          username,
          java.time.OffsetDateTime.now(),
        ),
      )
    def removeDismissal(activityId: Long, username: String): Task[Unit] = ZIO.unit

  private object NoOpTimeSlotRepository extends co.wtf.openspaces.db.TimeSlotRepository:
    def findAll: Task[Vector[co.wtf.openspaces.db.TimeSlotRow]] = ZIO.succeed(Vector.empty)
    def findById(id: Int): Task[Option[co.wtf.openspaces.db.TimeSlotRow]] = ZIO.none
    def findByRoomId(roomId: Int): Task[Vector[co.wtf.openspaces.db.TimeSlotRow]] = ZIO.succeed(Vector.empty)
    def findStartBounds: Task[Option[(java.time.LocalDateTime, java.time.LocalDateTime)]] =
      ZIO.succeed(Some((java.time.LocalDateTime.now().minusDays(1), java.time.LocalDateTime.now().plusDays(1))))

  private object NoOpHackathonProjectRepository
      extends co.wtf.openspaces.db.HackathonProjectRepository:
    def findById(
      id: Long,
    ): Task[Option[co.wtf.openspaces.db.HackathonProjectRow]] = ZIO.none
    def findAllActive: Task[Vector[co.wtf.openspaces.db.HackathonProjectRow]] =
      ZIO.succeed(Vector.empty)
    def insert(row: co.wtf.openspaces.db.HackathonProjectRow): Task[Unit] =
      ZIO.unit
    def update(row: co.wtf.openspaces.db.HackathonProjectRow): Task[Unit] =
      ZIO.unit
    def updateOwner(id: Long, newOwner: String): Task[Unit] = ZIO.unit
    def softDelete(id: Long): Task[Unit] = ZIO.unit
    def updateSlackThread(
      id: Long,
      channelId: String,
      threadTs: String,
      permalink: String,
    ): Task[Unit] = ZIO.unit

  private object NoOpHackathonProjectMemberRepository
      extends co.wtf.openspaces.db.HackathonProjectMemberRepository:
    def findActiveByProject(
      projectId: Long,
    ): Task[Vector[co.wtf.openspaces.db.HackathonProjectMemberRow]] =
      ZIO.succeed(Vector.empty)
    def findActiveByUser(
      username: String,
    ): Task[Option[co.wtf.openspaces.db.HackathonProjectMemberRow]] =
      ZIO.none
    def addMember(
      projectId: Long,
      username: String,
    ): Task[co.wtf.openspaces.db.HackathonProjectMemberRow] =
      ZIO.succeed(
        co.wtf.openspaces.db.HackathonProjectMemberRow(
          projectId,
          username,
          java.time.OffsetDateTime.now(),
          None,
        ),
      )
    def removeMember(projectId: Long, username: String): Task[Unit] = ZIO.unit

  private object NoOpConfirmedActionRepository
      extends co.wtf.openspaces.db.ConfirmedActionRepository:
    def append(
      entityType: String,
      actionType: String,
      payload: String,
      actor: Option[String],
    ): Task[co.wtf.openspaces.db.ConfirmedActionRow] =
      ZIO.succeed(
        co.wtf.openspaces.db.ConfirmedActionRow(
          0L,
          java.time.OffsetDateTime.now(),
          entityType,
          actionType,
          payload,
          actor,
        ),
      )
    def findAll: Task[Vector[co.wtf.openspaces.db.ConfirmedActionRow]] =
      ZIO.succeed(Vector.empty)
    def deleteByActors(actors: List[String]): Task[Int] = ZIO.succeed(0)
    def truncate: Task[Unit] = ZIO.unit

  private object NoOpDiscussionRepository
      extends co.wtf.openspaces.db.DiscussionRepository:
    def findById(id: Long): Task[Option[co.wtf.openspaces.db.DiscussionRow]] = ZIO.none
    def findAllActive: Task[Vector[co.wtf.openspaces.db.DiscussionRow]] = ZIO.succeed(Vector.empty)
    def insert(row: co.wtf.openspaces.db.DiscussionRow): Task[Unit] = ZIO.unit
    def update(row: co.wtf.openspaces.db.DiscussionRow): Task[Unit] = ZIO.unit
    def softDelete(id: Long): Task[Boolean] = ZIO.succeed(true)
    def truncate: Task[Unit] = ZIO.unit
    def updateSlackThread(topicId: Long, channelId: String, threadTs: String, permalink: String): Task[Unit] = ZIO.unit
    def updateRoomSlot(topicId: Long, roomId: Option[Int], timeSlotId: Option[Int]): Task[Unit] = ZIO.unit