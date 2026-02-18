package co.wtf.openspaces.db

import com.augustnagro.magnum.*
import zio.json.*

import java.time.OffsetDateTime

// User entity
case class UserRow(
  githubUsername: String,
  displayName: Option[String],
  createdAt: OffsetDateTime
) derives DbCodec, JsonCodec

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
  createdAt: OffsetDateTime,
  updatedAt: OffsetDateTime,
  deletedAt: Option[OffsetDateTime],
  slackChannelId: Option[String],
  slackThreadTs: Option[String],
  slackPermalink: Option[String]
) derives DbCodec

object DiscussionRow:
  def create(
    id: Long,
    topic: String,
    facilitator: String,
    glyphicon: String,
    roomSlot: Option[String]
  ): DiscussionRow =
    val now = OffsetDateTime.now()
    DiscussionRow(id, topic, facilitator, glyphicon, roomSlot, now, now, None, None, None, None)

case class TopicVoteRow(
  topicId: Long,
  githubUsername: String,
  position: String,
  firstVotedAt: OffsetDateTime
) derives DbCodec

case class LightningTalkRow(
  id: Long,
  speaker: String,
  assignmentNight: Option[String],
  assignmentSlot: Option[Int],
  createdAt: OffsetDateTime,
  deletedAt: Option[OffsetDateTime],
  slackChannelId: Option[String],
  slackThreadTs: Option[String],
  slackPermalink: Option[String],
) derives DbCodec

// Hackathon Projects (Wednesday hackday)

case class HackathonProjectRow(
  id: Long,
  title: String,
  owner: String,
  createdAt: OffsetDateTime,
  deletedAt: Option[OffsetDateTime],
  slackChannelId: Option[String],
  slackThreadTs: Option[String],
  slackPermalink: Option[String],
) derives DbCodec

case class HackathonProjectMemberRow(
  projectId: Long,
  githubUsername: String,
  joinedAt: OffsetDateTime,
  leftAt: Option[OffsetDateTime],
) derives DbCodec
