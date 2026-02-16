package co.wtf.openspaces.db

import com.augustnagro.magnum.*
import co.wtf.openspaces.VotePosition
import zio.*

import java.time.OffsetDateTime
import javax.sql.DataSource

// Helper to run Magnum transactions in ZIO
def transactZIO[A](ds: DataSource)(f: DbCon ?=> A): Task[A] =
  ZIO.attempt(transact(ds)(f))

trait UserRepository:
  def findByUsername(username: String): Task[Option[UserRow]]
  def upsert(username: String, displayName: Option[String]): Task[UserRow]
  def findAll: Task[Vector[UserRow]]

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
          if row.displayName != displayName then
            sql"""UPDATE users SET display_name = $displayName WHERE github_username = $username""".update.run()
            row.copy(displayName = displayName)
          else row
        case None =>
          val now = OffsetDateTime.now()
          sql"""INSERT INTO users (github_username, display_name, created_at) VALUES ($username, $displayName, $now)""".update.run()
          UserRow(username, displayName, now)

  def findAll: Task[Vector[UserRow]] =
    transactZIO(ds):
      sql"SELECT github_username, display_name, created_at FROM users".query[UserRow].run()

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

class DiscussionRepositoryLive(ds: DataSource) extends DiscussionRepository:
  def findById(id: Long): Task[Option[DiscussionRow]] =
    transactZIO(ds):
      sql"""SELECT id, topic, facilitator, glyphicon, room_slot::text,
                   created_at, updated_at, deleted_at,
                   slack_channel_id, slack_thread_ts, slack_permalink
            FROM discussions WHERE id = $id"""
        .query[DiscussionRow]
        .run()
        .headOption

  def findAllActive: Task[Vector[DiscussionRow]] =
    transactZIO(ds):
      sql"""SELECT id, topic, facilitator, glyphicon, room_slot::text,
                   created_at, updated_at, deleted_at,
                   slack_channel_id, slack_thread_ts, slack_permalink
            FROM discussions WHERE deleted_at IS NULL"""
        .query[DiscussionRow]
        .run()

  def insert(row: DiscussionRow): Task[Unit] =
    transactZIO(ds):
      sql"""INSERT INTO discussions (id, topic, facilitator, glyphicon, room_slot, created_at, updated_at, deleted_at, slack_channel_id, slack_thread_ts, slack_permalink)
            VALUES (${row.id}, ${row.topic}, ${row.facilitator}, ${row.glyphicon},
                    ${row.roomSlot}::jsonb,
                    ${row.createdAt}, ${row.updatedAt}, ${row.deletedAt},
                    ${row.slackChannelId}, ${row.slackThreadTs}, ${row.slackPermalink})""".update.run()
      ()

  def update(row: DiscussionRow): Task[Unit] =
    transactZIO(ds):
      sql"""UPDATE discussions SET
              topic = ${row.topic},
              facilitator = ${row.facilitator},
              glyphicon = ${row.glyphicon},
              room_slot = ${row.roomSlot}::jsonb,
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
  def findBySpeakerActive(speaker: String): Task[Option[LightningTalkRow]]
  def findAllActive: Task[Vector[LightningTalkRow]]
  def insert(row: LightningTalkRow): Task[Unit]
  def update(row: LightningTalkRow): Task[Unit]
  def softDelete(id: Long): Task[Unit]
  def updateSlackThread(proposalId: Long, channelId: String, threadTs: String, permalink: String): Task[Unit]

class LightningTalkRepositoryLive(ds: DataSource) extends LightningTalkRepository:
  def findById(id: Long): Task[Option[LightningTalkRow]] =
    transactZIO(ds):
      sql"""SELECT id, topic, speaker, assignment_night, assignment_slot,
                   created_at, updated_at, deleted_at,
                   slack_channel_id, slack_thread_ts, slack_permalink
            FROM lightning_talks
            WHERE id = $id"""
        .query[LightningTalkRow]
        .run()
        .headOption

  def findBySpeakerActive(speaker: String): Task[Option[LightningTalkRow]] =
    transactZIO(ds):
      sql"""SELECT id, topic, speaker, assignment_night, assignment_slot,
                   created_at, updated_at, deleted_at,
                   slack_channel_id, slack_thread_ts, slack_permalink
            FROM lightning_talks
            WHERE speaker = $speaker
              AND deleted_at IS NULL"""
        .query[LightningTalkRow]
        .run()
        .headOption

  def findAllActive: Task[Vector[LightningTalkRow]] =
    transactZIO(ds):
      sql"""SELECT id, topic, speaker, assignment_night, assignment_slot,
                   created_at, updated_at, deleted_at,
                   slack_channel_id, slack_thread_ts, slack_permalink
            FROM lightning_talks
            WHERE deleted_at IS NULL"""
        .query[LightningTalkRow]
        .run()

  def insert(row: LightningTalkRow): Task[Unit] =
    transactZIO(ds):
      sql"""INSERT INTO lightning_talks (
              id, topic, speaker, assignment_night, assignment_slot,
              created_at, updated_at, deleted_at,
              slack_channel_id, slack_thread_ts, slack_permalink
            ) VALUES (
              ${row.id}, ${row.topic}, ${row.speaker},
              ${row.assignmentNight}, ${row.assignmentSlot},
              ${row.createdAt}, ${row.updatedAt}, ${row.deletedAt},
              ${row.slackChannelId}, ${row.slackThreadTs}, ${row.slackPermalink}
            )""".update.run()
      ()

  def update(row: LightningTalkRow): Task[Unit] =
    transactZIO(ds):
      sql"""UPDATE lightning_talks SET
              topic = ${row.topic},
              speaker = ${row.speaker},
              assignment_night = ${row.assignmentNight},
              assignment_slot = ${row.assignmentSlot},
              deleted_at = ${row.deletedAt}
            WHERE id = ${row.id}""".update.run()
      ()

  def softDelete(id: Long): Task[Unit] =
    transactZIO(ds):
      sql"""UPDATE lightning_talks
            SET deleted_at = NOW()
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
