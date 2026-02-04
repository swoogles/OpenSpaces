package co.wtf.openspaces.db

import com.augustnagro.magnum.*
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

class DiscussionRepositoryLive(ds: DataSource) extends DiscussionRepository:
  def findById(id: Long): Task[Option[DiscussionRow]] =
    transactZIO(ds):
      sql"""SELECT id, topic, facilitator, glyphicon, room_slot::text, interested_parties::text, 
                   created_at, updated_at, deleted_at 
            FROM discussions WHERE id = $id"""
        .query[DiscussionRow]
        .run()
        .headOption

  def findAllActive: Task[Vector[DiscussionRow]] =
    transactZIO(ds):
      sql"""SELECT id, topic, facilitator, glyphicon, room_slot::text, interested_parties::text, 
                   created_at, updated_at, deleted_at 
            FROM discussions WHERE deleted_at IS NULL"""
        .query[DiscussionRow]
        .run()

  def insert(row: DiscussionRow): Task[Unit] =
    transactZIO(ds):
      sql"""INSERT INTO discussions (id, topic, facilitator, glyphicon, room_slot, interested_parties, created_at, updated_at, deleted_at)
            VALUES (${row.id}, ${row.topic}, ${row.facilitator}, ${row.glyphicon}, 
                    ${row.roomSlot}::jsonb, ${row.interestedParties}::jsonb, 
                    ${row.createdAt}, ${row.updatedAt}, ${row.deletedAt})""".update.run()
      ()

  def update(row: DiscussionRow): Task[Unit] =
    transactZIO(ds):
      sql"""UPDATE discussions SET 
              topic = ${row.topic}, 
              facilitator = ${row.facilitator}, 
              glyphicon = ${row.glyphicon}, 
              room_slot = ${row.roomSlot}::jsonb, 
              interested_parties = ${row.interestedParties}::jsonb,
              deleted_at = ${row.deletedAt}
            WHERE id = ${row.id}""".update.run()
      ()

  def softDelete(id: Long): Task[Unit] =
    transactZIO(ds):
      sql"UPDATE discussions SET deleted_at = NOW() WHERE id = $id".update.run()
      ()

  def truncate: Task[Unit] =
    transactZIO(ds):
      sql"TRUNCATE discussions".update.run()
      ()

object DiscussionRepository:
  val layer: ZLayer[DataSource, Nothing, DiscussionRepository] =
    ZLayer.fromFunction(ds => DiscussionRepositoryLive(ds))
