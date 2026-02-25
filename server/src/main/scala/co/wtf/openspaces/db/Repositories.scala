package co.wtf.openspaces.db

import com.augustnagro.magnum.*
import co.wtf.openspaces.discussions.VotePosition
import zio.*

import java.time.OffsetDateTime
import javax.sql.DataSource

// Helper to run Magnum transactions in ZIO
def transactZIO[A](ds: DataSource)(f: DbCon ?=> A): Task[A] =
  ZIO.attempt(transact(ds)(f))

// Room Repository

trait RoomRepository:
  def findAll: Task[Vector[RoomRow]]
  def findById(id: Int): Task[Option[RoomRow]]

class RoomRepositoryLive(ds: DataSource) extends RoomRepository:
  def findAll: Task[Vector[RoomRow]] =
    transactZIO(ds):
      sql"SELECT id, name, capacity FROM rooms ORDER BY id".query[RoomRow].run()

  def findById(id: Int): Task[Option[RoomRow]] =
    transactZIO(ds):
      sql"SELECT id, name, capacity FROM rooms WHERE id = $id".query[RoomRow].run().headOption

object RoomRepository:
  val layer: ZLayer[DataSource, Nothing, RoomRepository] =
    ZLayer.fromFunction(ds => RoomRepositoryLive(ds))

// Time Slot Repository

trait TimeSlotRepository:
  def findAll: Task[Vector[TimeSlotRow]]
  def findById(id: Int): Task[Option[TimeSlotRow]]
  def findByRoomId(roomId: Int): Task[Vector[TimeSlotRow]]
  def findStartBounds: Task[Option[(java.time.LocalDateTime, java.time.LocalDateTime)]]

class TimeSlotRepositoryLive(ds: DataSource) extends TimeSlotRepository:
  def findAll: Task[Vector[TimeSlotRow]] =
    transactZIO(ds):
      sql"SELECT id, room_id, start_time, end_time FROM time_slots ORDER BY start_time, room_id"
        .query[TimeSlotRow]
        .run()

  def findById(id: Int): Task[Option[TimeSlotRow]] =
    transactZIO(ds):
      sql"SELECT id, room_id, start_time, end_time FROM time_slots WHERE id = $id"
        .query[TimeSlotRow]
        .run()
        .headOption

  def findByRoomId(roomId: Int): Task[Vector[TimeSlotRow]] =
    transactZIO(ds):
      sql"SELECT id, room_id, start_time, end_time FROM time_slots WHERE room_id = $roomId ORDER BY start_time"
        .query[TimeSlotRow]
        .run()

  def findStartBounds: Task[Option[(java.time.LocalDateTime, java.time.LocalDateTime)]] =
    transactZIO(ds):
      sql"SELECT MIN(start_time), MAX(start_time) FROM time_slots"
        .query[(Option[java.time.LocalDateTime], Option[java.time.LocalDateTime])]
        .run()
        .headOption
        .flatMap { case (minStart, maxStart) =>
          minStart.zip(maxStart)
        }

object TimeSlotRepository:
  val layer: ZLayer[DataSource, Nothing, TimeSlotRepository] =
    ZLayer.fromFunction(ds => TimeSlotRepositoryLive(ds))

// User Repository

trait UserRepository:
  def findByUsername(username: String): Task[Option[UserRow]]
  def upsert(username: String, displayName: Option[String]): Task[UserRow]
  def findAll: Task[Vector[UserRow]]
  def deleteByUsernames(usernames: List[String]): Task[Int]

class UserRepositoryLive(ds: DataSource) extends UserRepository:
  def findByUsername(username: String): Task[Option[UserRow]] =
    transactZIO(ds):
      sql"SELECT github_username, display_name, created_at FROM users WHERE github_username = $username"
        .query[UserRow]
        .run()
        .headOption

  def upsert(username: String, displayName: Option[String]): Task[UserRow] =
    transactZIO(ds):
      val existing = sql"SELECT github_username, display_name, created_at FROM users WHERE github_username = $username"
        .query[UserRow]
        .run()
        .headOption
      
      existing match
        case Some(row) =>
          val normalizedDisplayName = displayName.map(_.trim).filter(_.nonEmpty)
          normalizedDisplayName match
            case Some(value) if row.displayName != Some(value) =>
              sql"""UPDATE users SET display_name = ${Some(value)} WHERE github_username = $username""".update.run()
              row.copy(displayName = Some(value))
            case _ =>
              row
        case None =>
          val now = OffsetDateTime.now()
          val normalizedDisplayName = displayName.map(_.trim).filter(_.nonEmpty)
          sql"""INSERT INTO users (github_username, display_name, created_at) VALUES ($username, $normalizedDisplayName, $now)""".update.run()
          UserRow(username, normalizedDisplayName, now)

  def findAll: Task[Vector[UserRow]] =
    transactZIO(ds):
      sql"SELECT github_username, display_name, created_at FROM users".query[UserRow].run()

  def deleteByUsernames(usernames: List[String]): Task[Int] =
    transactZIO(ds):
      usernames.distinct.foldLeft(0) { (deletedCount, username) =>
        deletedCount + sql"DELETE FROM users WHERE github_username = $username".update.run()
      }

object UserRepository:
  val layer: ZLayer[DataSource, Nothing, UserRepository] =
    ZLayer.fromFunction(ds => UserRepositoryLive(ds))

trait EventRepository:
  def append(eventType: String, topicId: Long, payload: String, actor: String): Task[DiscussionEventRow]
  def findByTopicId(topicId: Long): Task[Vector[DiscussionEventRow]]
  def findByActor(actor: String): Task[Vector[DiscussionEventRow]]
  def findAll: Task[Vector[DiscussionEventRow]]
  def findAllOrdered: Task[Vector[DiscussionEventRow]]

class EventRepositoryLive(ds: DataSource) extends EventRepository:
  def append(eventType: String, topicId: Long, payload: String, actor: String): Task[DiscussionEventRow] =
    transactZIO(ds):
      val now = OffsetDateTime.now()
      val id = sql"""
        INSERT INTO discussion_events (event_type, topic_id, payload, actor, created_at) 
        VALUES ($eventType, $topicId, $payload::jsonb, $actor, $now) 
        RETURNING id
      """.query[Long].run().head
      DiscussionEventRow(id, eventType, topicId, payload, actor, now)

  def findByTopicId(topicId: Long): Task[Vector[DiscussionEventRow]] =
    transactZIO(ds):
      sql"""SELECT id, event_type, topic_id, payload::text, actor, created_at 
            FROM discussion_events WHERE topic_id = $topicId ORDER BY created_at"""
        .query[DiscussionEventRow]
        .run()

  def findByActor(actor: String): Task[Vector[DiscussionEventRow]] =
    transactZIO(ds):
      sql"""SELECT id, event_type, topic_id, payload::text, actor, created_at 
            FROM discussion_events WHERE actor = $actor ORDER BY created_at"""
        .query[DiscussionEventRow]
        .run()

  def findAll: Task[Vector[DiscussionEventRow]] =
    transactZIO(ds):
      sql"SELECT id, event_type, topic_id, payload::text, actor, created_at FROM discussion_events"
        .query[DiscussionEventRow]
        .run()

  def findAllOrdered: Task[Vector[DiscussionEventRow]] =
    transactZIO(ds):
      sql"""SELECT id, event_type, topic_id, payload::text, actor, created_at 
            FROM discussion_events ORDER BY created_at, id"""
        .query[DiscussionEventRow]
        .run()

object EventRepository:
  val layer: ZLayer[DataSource, Nothing, EventRepository] =
    ZLayer.fromFunction(ds => EventRepositoryLive(ds))

trait DiscussionRepository:
  def findById(id: Long): Task[Option[DiscussionRow]]
  def findAllActive: Task[Vector[DiscussionRow]]
  def insert(row: DiscussionRow): Task[Unit]
  def update(row: DiscussionRow): Task[Unit]
  def softDelete(id: Long): Task[Unit]
  def truncate: Task[Unit]
  def updateSlackThread(topicId: Long, channelId: String, threadTs: String, permalink: String): Task[Unit]
  def updateRoomSlot(topicId: Long, roomId: Option[Int], timeSlotId: Option[Int]): Task[Unit]

class DiscussionRepositoryLive(ds: DataSource) extends DiscussionRepository:
  def findById(id: Long): Task[Option[DiscussionRow]] =
    transactZIO(ds):
      sql"""SELECT id, topic, facilitator, glyphicon,
                   room_id, time_slot_id,
                   is_locked_timeslot,
                   created_at, updated_at, deleted_at,
                   slack_channel_id, slack_thread_ts, slack_permalink
            FROM discussions WHERE id = $id"""
        .query[DiscussionRow]
        .run()
        .headOption

  def findAllActive: Task[Vector[DiscussionRow]] =
    transactZIO(ds):
      sql"""SELECT id, topic, facilitator, glyphicon,
                   room_id, time_slot_id,
                   is_locked_timeslot,
                   created_at, updated_at, deleted_at,
                   slack_channel_id, slack_thread_ts, slack_permalink
            FROM discussions WHERE deleted_at IS NULL"""
        .query[DiscussionRow]
        .run()

  def insert(row: DiscussionRow): Task[Unit] =
    transactZIO(ds):
      sql"""INSERT INTO discussions (id, topic, facilitator, glyphicon,
                   room_id, time_slot_id,
                   is_locked_timeslot, created_at, updated_at, deleted_at,
                   slack_channel_id, slack_thread_ts, slack_permalink)
            VALUES (${row.id}, ${row.topic}, ${row.facilitator}, ${row.glyphicon},
                    ${row.roomId}, ${row.timeSlotId},
                    ${row.isLockedTimeslot},
                    ${row.createdAt}, ${row.updatedAt}, ${row.deletedAt},
                    ${row.slackChannelId}, ${row.slackThreadTs}, ${row.slackPermalink})""".update.run()
      ()

  def update(row: DiscussionRow): Task[Unit] =
    transactZIO(ds):
      sql"""UPDATE discussions SET
              topic = ${row.topic},
              facilitator = ${row.facilitator},
              glyphicon = ${row.glyphicon},
              room_id = ${row.roomId},
              time_slot_id = ${row.timeSlotId},
              is_locked_timeslot = ${row.isLockedTimeslot},
              deleted_at = ${row.deletedAt}
            WHERE id = ${row.id}""".update.run()
      ()

  def softDelete(id: Long): Task[Unit] =
    transactZIO(ds):
      sql"UPDATE discussions SET deleted_at = NOW() WHERE id = $id".update.run()
      ()

  def truncate: Task[Unit] =
    transactZIO(ds):
      sql"TRUNCATE discussions CASCADE".update.run()
      ()

  def updateSlackThread(topicId: Long, channelId: String, threadTs: String, permalink: String): Task[Unit] =
    transactZIO(ds):
      sql"""UPDATE discussions SET
              slack_channel_id = $channelId,
              slack_thread_ts = $threadTs,
              slack_permalink = $permalink
            WHERE id = $topicId""".update.run()
      ()

  def updateRoomSlot(topicId: Long, roomId: Option[Int], timeSlotId: Option[Int]): Task[Unit] =
    transactZIO(ds):
      sql"""UPDATE discussions SET
              room_id = $roomId,
              time_slot_id = $timeSlotId
            WHERE id = $topicId""".update.run()
      ()

object DiscussionRepository:
  val layer: ZLayer[DataSource, Nothing, DiscussionRepository] =
    ZLayer.fromFunction(ds => DiscussionRepositoryLive(ds))

trait TopicVoteRepository:
  def findAllForActiveDiscussions: Task[Vector[TopicVoteRow]]
  def upsertVote(topicId: Long, githubUsername: String, position: VotePosition): Task[TopicVoteRow]
  def deleteVote(topicId: Long, githubUsername: String): Task[Unit]

class TopicVoteRepositoryLive(ds: DataSource) extends TopicVoteRepository:
  def findAllForActiveDiscussions: Task[Vector[TopicVoteRow]] =
    transactZIO(ds):
      sql"""SELECT tv.topic_id, tv.github_username, tv.position, tv.first_voted_at
            FROM topic_votes tv
            INNER JOIN discussions d ON d.id = tv.topic_id
            WHERE d.deleted_at IS NULL"""
        .query[TopicVoteRow]
        .run()

  def upsertVote(topicId: Long, githubUsername: String, position: VotePosition): Task[TopicVoteRow] =
    transactZIO(ds):
      sql"""INSERT INTO topic_votes (topic_id, github_username, position)
            VALUES ($topicId, $githubUsername, ${position.toString})
            ON CONFLICT (topic_id, github_username)
            DO UPDATE SET position = EXCLUDED.position""".update.run()
      sql"""SELECT topic_id, github_username, position, first_voted_at
            FROM topic_votes
            WHERE topic_id = $topicId AND github_username = $githubUsername"""
        .query[TopicVoteRow]
        .run()
        .head

  def deleteVote(topicId: Long, githubUsername: String): Task[Unit] =
    transactZIO(ds):
      sql"""DELETE FROM topic_votes
            WHERE topic_id = $topicId AND github_username = $githubUsername""".update.run()
      ()

object TopicVoteRepository:
  val layer: ZLayer[DataSource, Nothing, TopicVoteRepository] =
    ZLayer.fromFunction(ds => TopicVoteRepositoryLive(ds))

trait LightningTalkRepository:
  def findById(id: Long): Task[Option[LightningTalkRow]]
  def findBySpeaker(speaker: String): Task[Option[LightningTalkRow]]
  def findAll: Task[Vector[LightningTalkRow]]
  def insert(row: LightningTalkRow): Task[Unit]
  def update(row: LightningTalkRow): Task[Unit]
  def delete(id: Long): Task[Unit]
  def updateSlackThread(proposalId: Long, channelId: String, threadTs: String, permalink: String): Task[Unit]

class LightningTalkRepositoryLive(ds: DataSource) extends LightningTalkRepository:
  def findById(id: Long): Task[Option[LightningTalkRow]] =
    transactZIO(ds):
      sql"""SELECT id, speaker, assignment_night, assignment_slot,
                   created_at,
                   slack_channel_id, slack_thread_ts, slack_permalink
            FROM lightning_talks
            WHERE id = $id"""
        .query[LightningTalkRow]
        .run()
        .headOption

  def findBySpeaker(speaker: String): Task[Option[LightningTalkRow]] =
    transactZIO(ds):
      sql"""SELECT id, speaker, assignment_night, assignment_slot,
                   created_at,
                   slack_channel_id, slack_thread_ts, slack_permalink
            FROM lightning_talks
            WHERE speaker = $speaker"""
        .query[LightningTalkRow]
        .run()
        .headOption

  def findAll: Task[Vector[LightningTalkRow]] =
    transactZIO(ds):
      sql"""SELECT id, speaker, assignment_night, assignment_slot,
                   created_at,
                   slack_channel_id, slack_thread_ts, slack_permalink
            FROM lightning_talks"""
        .query[LightningTalkRow]
        .run()

  def insert(row: LightningTalkRow): Task[Unit] =
    transactZIO(ds):
      sql"""INSERT INTO lightning_talks (
              id, speaker, assignment_night, assignment_slot,
              created_at,
              slack_channel_id, slack_thread_ts, slack_permalink
            ) VALUES (
              ${row.id}, ${row.speaker},
              ${row.assignmentNight}, ${row.assignmentSlot},
              ${row.createdAt},
              ${row.slackChannelId}, ${row.slackThreadTs}, ${row.slackPermalink}
            )""".update.run()
      ()

  def update(row: LightningTalkRow): Task[Unit] =
    transactZIO(ds):
      sql"""UPDATE lightning_talks SET
              assignment_night = ${row.assignmentNight},
              assignment_slot = ${row.assignmentSlot}
            WHERE id = ${row.id}""".update.run()
      ()

  def delete(id: Long): Task[Unit] =
    transactZIO(ds):
      sql"""DELETE FROM lightning_talks
            WHERE id = $id""".update.run()
      ()

  def updateSlackThread(proposalId: Long, channelId: String, threadTs: String, permalink: String): Task[Unit] =
    transactZIO(ds):
      sql"""UPDATE lightning_talks SET
              slack_channel_id = $channelId,
              slack_thread_ts = $threadTs,
              slack_permalink = $permalink
            WHERE id = $proposalId""".update.run()
      ()

object LightningTalkRepository:
  val layer: ZLayer[DataSource, Nothing, LightningTalkRepository] =
    ZLayer.fromFunction(ds => LightningTalkRepositoryLive(ds))

// Hackathon Projects (Wednesday hackday)

trait HackathonProjectRepository:
  def findById(id: Long): Task[Option[HackathonProjectRow]]
  def findAllActive: Task[Vector[HackathonProjectRow]]
  def insert(row: HackathonProjectRow): Task[Unit]
  def update(row: HackathonProjectRow): Task[Unit]
  def updateOwner(id: Long, newOwner: String): Task[Unit]
  def softDelete(id: Long): Task[Unit]
  def updateSlackThread(id: Long, channelId: String, threadTs: String, permalink: String): Task[Unit]

class HackathonProjectRepositoryLive(ds: DataSource) extends HackathonProjectRepository:
  def findById(id: Long): Task[Option[HackathonProjectRow]] =
    transactZIO(ds):
      sql"""SELECT id, title, owner, created_at, deleted_at,
                   slack_channel_id, slack_thread_ts, slack_permalink
            FROM hackathon_projects
            WHERE id = $id"""
        .query[HackathonProjectRow]
        .run()
        .headOption

  def findAllActive: Task[Vector[HackathonProjectRow]] =
    transactZIO(ds):
      sql"""SELECT id, title, owner, created_at, deleted_at,
                   slack_channel_id, slack_thread_ts, slack_permalink
            FROM hackathon_projects
            WHERE deleted_at IS NULL"""
        .query[HackathonProjectRow]
        .run()

  def insert(row: HackathonProjectRow): Task[Unit] =
    transactZIO(ds):
      sql"""INSERT INTO hackathon_projects (
              id, title, owner, created_at, deleted_at,
              slack_channel_id, slack_thread_ts, slack_permalink
            ) VALUES (
              ${row.id}, ${row.title}, ${row.owner},
              ${row.createdAt}, ${row.deletedAt},
              ${row.slackChannelId}, ${row.slackThreadTs}, ${row.slackPermalink}
            )""".update.run()
      ()

  def update(row: HackathonProjectRow): Task[Unit] =
    transactZIO(ds):
      sql"""UPDATE hackathon_projects SET
              title = ${row.title},
              owner = ${row.owner},
              deleted_at = ${row.deletedAt}
            WHERE id = ${row.id}""".update.run()
      ()

  def updateOwner(id: Long, newOwner: String): Task[Unit] =
    transactZIO(ds):
      sql"""UPDATE hackathon_projects SET owner = $newOwner WHERE id = $id""".update.run()
      ()

  def softDelete(id: Long): Task[Unit] =
    transactZIO(ds):
      sql"""UPDATE hackathon_projects SET deleted_at = NOW() WHERE id = $id""".update.run()
      ()

  def updateSlackThread(id: Long, channelId: String, threadTs: String, permalink: String): Task[Unit] =
    transactZIO(ds):
      sql"""UPDATE hackathon_projects SET
              slack_channel_id = $channelId,
              slack_thread_ts = $threadTs,
              slack_permalink = $permalink
            WHERE id = $id""".update.run()
      ()

object HackathonProjectRepository:
  val layer: ZLayer[DataSource, Nothing, HackathonProjectRepository] =
    ZLayer.fromFunction(ds => HackathonProjectRepositoryLive(ds))

trait HackathonProjectMemberRepository:
  def findActiveByProject(projectId: Long): Task[Vector[HackathonProjectMemberRow]]
  def findActiveByUser(username: String): Task[Option[HackathonProjectMemberRow]]
  def addMember(projectId: Long, username: String): Task[HackathonProjectMemberRow]
  def removeMember(projectId: Long, username: String): Task[Unit]

class HackathonProjectMemberRepositoryLive(ds: DataSource) extends HackathonProjectMemberRepository:
  def findActiveByProject(projectId: Long): Task[Vector[HackathonProjectMemberRow]] =
    transactZIO(ds):
      sql"""SELECT project_id, github_username, joined_at, left_at
            FROM hackathon_project_members
            WHERE project_id = $projectId AND left_at IS NULL
            ORDER BY joined_at"""
        .query[HackathonProjectMemberRow]
        .run()

  def findActiveByUser(username: String): Task[Option[HackathonProjectMemberRow]] =
    transactZIO(ds):
      sql"""SELECT hpm.project_id, hpm.github_username, hpm.joined_at, hpm.left_at
            FROM hackathon_project_members hpm
            INNER JOIN hackathon_projects hp ON hp.id = hpm.project_id
            WHERE hpm.github_username = $username
              AND hpm.left_at IS NULL
              AND hp.deleted_at IS NULL"""
        .query[HackathonProjectMemberRow]
        .run()
        .headOption

  def addMember(projectId: Long, username: String): Task[HackathonProjectMemberRow] =
    transactZIO(ds):
      val now = OffsetDateTime.now()
      // UPSERT: if member previously left this project, rejoin by clearing left_at
      sql"""INSERT INTO hackathon_project_members (project_id, github_username, joined_at)
            VALUES ($projectId, $username, $now)
            ON CONFLICT (project_id, github_username)
            DO UPDATE SET joined_at = $now, left_at = NULL""".update.run()
      HackathonProjectMemberRow(projectId, username, now, None)

  def removeMember(projectId: Long, username: String): Task[Unit] =
    transactZIO(ds):
      sql"""UPDATE hackathon_project_members
            SET left_at = NOW()
            WHERE project_id = $projectId
              AND github_username = $username
              AND left_at IS NULL""".update.run()
      ()

object HackathonProjectMemberRepository:
  val layer: ZLayer[DataSource, Nothing, HackathonProjectMemberRepository] =
    ZLayer.fromFunction(ds => HackathonProjectMemberRepositoryLive(ds))

// Activities

trait ActivityRepository:
  def findById(id: Long): Task[Option[ActivityRow]]
  def findAllActive: Task[Vector[ActivityRow]]
  def insert(row: ActivityRow): Task[Unit]
  def update(row: ActivityRow): Task[Unit]
  def updateOwner(id: Long, newOwner: String): Task[Unit]
  def softDelete(id: Long): Task[Unit]
  def updateSlackThread(id: Long, channelId: String, threadTs: String, permalink: String): Task[Unit]

class ActivityRepositoryLive(ds: DataSource) extends ActivityRepository:
  def findById(id: Long): Task[Option[ActivityRow]] =
    transactZIO(ds):
      sql"""SELECT id, description, creator, event_time,
                   created_at, updated_at, deleted_at,
                   slack_channel_id, slack_thread_ts, slack_permalink
            FROM activities
            WHERE id = $id"""
        .query[ActivityRow]
        .run()
        .headOption

  def findAllActive: Task[Vector[ActivityRow]] =
    transactZIO(ds):
      sql"""SELECT id, description, creator, event_time,
                   created_at, updated_at, deleted_at,
                   slack_channel_id, slack_thread_ts, slack_permalink
            FROM activities
            WHERE deleted_at IS NULL"""
        .query[ActivityRow]
        .run()

  def insert(row: ActivityRow): Task[Unit] =
    transactZIO(ds):
      sql"""INSERT INTO activities (
              id, description, creator, event_time,
              created_at, updated_at, deleted_at,
              slack_channel_id, slack_thread_ts, slack_permalink
            ) VALUES (
              ${row.id}, ${row.description}, ${row.creator}, ${row.eventTime},
              ${row.createdAt}, ${row.updatedAt}, ${row.deletedAt},
              ${row.slackChannelId}, ${row.slackThreadTs}, ${row.slackPermalink}
            )""".update.run()
      ()

  def update(row: ActivityRow): Task[Unit] =
    transactZIO(ds):
      sql"""UPDATE activities SET
              description = ${row.description},
              event_time = ${row.eventTime},
              updated_at = ${row.updatedAt},
              deleted_at = ${row.deletedAt}
            WHERE id = ${row.id}""".update.run()
      ()

  def updateOwner(id: Long, newOwner: String): Task[Unit] =
    transactZIO(ds):
      sql"""UPDATE activities SET creator = $newOwner, updated_at = NOW() WHERE id = $id""".update.run()
      ()

  def softDelete(id: Long): Task[Unit] =
    transactZIO(ds):
      sql"""UPDATE activities SET deleted_at = NOW(), updated_at = NOW() WHERE id = $id""".update.run()
      ()

  def updateSlackThread(id: Long, channelId: String, threadTs: String, permalink: String): Task[Unit] =
    transactZIO(ds):
      sql"""UPDATE activities SET
              slack_channel_id = $channelId,
              slack_thread_ts = $threadTs,
              slack_permalink = $permalink,
              updated_at = NOW()
            WHERE id = $id""".update.run()
      ()

object ActivityRepository:
  val layer: ZLayer[DataSource, Nothing, ActivityRepository] =
    ZLayer.fromFunction(ds => ActivityRepositoryLive(ds))

trait ActivityInterestRepository:
  def findByActivity(activityId: Long): Task[Vector[ActivityInterestRow]]
  def addInterest(activityId: Long, username: String): Task[ActivityInterestRow]
  def removeInterest(activityId: Long, username: String): Task[Unit]

class ActivityInterestRepositoryLive(ds: DataSource) extends ActivityInterestRepository:
  def findByActivity(activityId: Long): Task[Vector[ActivityInterestRow]] =
    transactZIO(ds):
      sql"""SELECT activity_id, github_username, interested_at
            FROM activity_interest
            WHERE activity_id = $activityId"""
        .query[ActivityInterestRow]
        .run()

  def addInterest(activityId: Long, username: String): Task[ActivityInterestRow] =
    transactZIO(ds):
      val now = OffsetDateTime.now()
      sql"""INSERT INTO activity_interest (activity_id, github_username, interested_at)
            VALUES ($activityId, $username, $now)
            ON CONFLICT (activity_id, github_username)
            DO UPDATE SET interested_at = $now""".update.run()
      ActivityInterestRow(activityId, username, now)

  def removeInterest(activityId: Long, username: String): Task[Unit] =
    transactZIO(ds):
      sql"""DELETE FROM activity_interest
            WHERE activity_id = $activityId AND github_username = $username""".update.run()
      ()

object ActivityInterestRepository:
  val layer: ZLayer[DataSource, Nothing, ActivityInterestRepository] =
    ZLayer.fromFunction(ds => ActivityInterestRepositoryLive(ds))

// Confirmed action log (for visualization and replay)

trait ConfirmedActionRepository:
  def append(entityType: String, actionType: String, payload: String, actor: Option[String]): Task[ConfirmedActionRow]
  def findAll: Task[Vector[ConfirmedActionRow]]
  def deleteByActors(actors: List[String]): Task[Int]
  def truncate: Task[Unit]

class ConfirmedActionRepositoryLive(ds: DataSource) extends ConfirmedActionRepository:
  def append(entityType: String, actionType: String, payload: String, actor: Option[String]): Task[ConfirmedActionRow] =
    transactZIO(ds):
      val now = OffsetDateTime.now()
      val id = sql"""
        INSERT INTO confirmed_actions (created_at, entity_type, action_type, payload, actor)
        VALUES ($now, $entityType, $actionType, $payload::jsonb, $actor)
        RETURNING id
      """.query[Long].run().head
      ConfirmedActionRow(id, now, entityType, actionType, payload, actor)

  def findAll: Task[Vector[ConfirmedActionRow]] =
    transactZIO(ds):
      sql"""SELECT id, created_at, entity_type, action_type, payload::text, actor
            FROM confirmed_actions
            ORDER BY id"""
        .query[ConfirmedActionRow]
        .run()

  def deleteByActors(actors: List[String]): Task[Int] =
    transactZIO(ds):
      actors.distinct.foldLeft(0) { (deletedCount, actor) =>
        deletedCount + sql"DELETE FROM confirmed_actions WHERE actor = $actor".update.run()
      }

  def truncate: Task[Unit] =
    transactZIO(ds):
      sql"TRUNCATE confirmed_actions".update.run()
      ()

object ConfirmedActionRepository:
  val layer: ZLayer[DataSource, Nothing, ConfirmedActionRepository] =
    ZLayer.fromFunction(ds => ConfirmedActionRepositoryLive(ds))
