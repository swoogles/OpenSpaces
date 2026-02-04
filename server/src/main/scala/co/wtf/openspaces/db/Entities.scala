package co.wtf.openspaces.db

import com.augustnagro.magnum.*
import zio.json.*

import java.time.OffsetDateTime

// User entity
case class UserRow(
  githubUsername: String,
  displayName: Option[String],
  createdAt: OffsetDateTime
) derives DbCodec

object UserRow:
  def create(githubUsername: String, displayName: Option[String]): UserRow =
    UserRow(githubUsername, displayName, OffsetDateTime.now())

// Discussion event entity (event sourcing log)
case class DiscussionEventRow(
  id: Long,
  eventType: String,
  topicId: Long,
  payload: String, // JSON string
  actor: String,
  createdAt: OffsetDateTime
) derives DbCodec

object DiscussionEventRow:
  def create(eventType: String, topicId: Long, payload: String, actor: String): DiscussionEventRow =
    DiscussionEventRow(0L, eventType, topicId, payload, actor, OffsetDateTime.now())

// Discussion entity (materialized state)
case class DiscussionRow(
  id: Long,
  topic: String,
  facilitator: String,
  glyphicon: String,
  roomSlot: Option[String],       // JSON string
  interestedParties: String,      // JSON array string
  createdAt: OffsetDateTime,
  updatedAt: OffsetDateTime,
  deletedAt: Option[OffsetDateTime]
) derives DbCodec

object DiscussionRow:
  def create(
    id: Long,
    topic: String,
    facilitator: String,
    glyphicon: String,
    roomSlot: Option[String],
    interestedParties: String
  ): DiscussionRow =
    val now = OffsetDateTime.now()
    DiscussionRow(id, topic, facilitator, glyphicon, roomSlot, interestedParties, now, now, None)
