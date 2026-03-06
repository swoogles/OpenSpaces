package co.wtf.openspaces.slack

import co.wtf.openspaces.db.{SlackRosterMessageRepository, UserRepository}
import co.wtf.openspaces.Person
import neotype.unwrap
import zio.*

trait SlackRosterService:
  /** Update or create the roster message for an entity's Slack thread */
  def updateRoster(
    entityType: String,
    entityId: Long,
    channelId: String,
    threadTs: String,
    interestedUsers: List[Person]
  ): Task[Unit]

  /** Delete the roster message for an entity (when entity is deleted) */
  def deleteRoster(entityType: String, entityId: Long): Task[Unit]

class SlackRosterServiceLive(
  slackClient: SlackClient,
  rosterRepo: SlackRosterMessageRepository,
  userRepo: UserRepository
) extends SlackRosterService:

  def updateRoster(
    entityType: String,
    entityId: Long,
    channelId: String,
    threadTs: String,
    interestedUsers: List[Person]
  ): Task[Unit] =
    val githubUsernames = interestedUsers.map(_.unwrap)
    for
      // Get Slack IDs for interested users
      slackIdMap <- userRepo.findUsersWithSlackIds(githubUsernames)
      // Find existing roster message or create new one
      existingRoster <- rosterRepo.findByEntity(entityType, entityId)
      // Build roster text with @mentions for users who have linked Slack
      _ <- {
        val rosterText = buildRosterText(interestedUsers, slackIdMap)
        existingRoster match
          case Some(roster) =>
            // Update existing message
            slackClient.updateReply(channelId, roster.rosterMessageTs, rosterText)
              .catchAll(err => ZIO.logWarning(s"Failed to update roster message: ${err.getMessage}"))
          case None if interestedUsers.nonEmpty =>
            // Create new roster message
            for
              ts <- slackClient.postReplyWithTs(channelId, threadTs, rosterText)
              _ <- rosterRepo.upsert(entityType, entityId, channelId, threadTs, ts)
            yield ()
          case None =>
            ZIO.unit // No users, no message needed
      }
    yield ()

  def deleteRoster(entityType: String, entityId: Long): Task[Unit] =
    for
      existing <- rosterRepo.findByEntity(entityType, entityId)
      _ <- existing match
        case Some(roster) =>
          slackClient.deleteMessage(roster.slackChannelId, roster.rosterMessageTs)
            .catchAll(err => ZIO.logWarning(s"Failed to delete roster message: ${err.getMessage}")) *>
          rosterRepo.delete(entityType, entityId)
        case None =>
          ZIO.unit
    yield ()

  private def buildRosterText(users: List[Person], slackIdMap: Map[String, String]): String =
    val mentions = users.flatMap { person =>
      slackIdMap.get(person.unwrap).map(id => s"<@$id>")
    }

    val unlinkedCount = users.count(p => !slackIdMap.contains(p.unwrap))

    val linkedPart = if mentions.nonEmpty then mentions.mkString(" ") else ""
    val unlinkedPart = if unlinkedCount > 0 then
      s" _(+ $unlinkedCount without Slack linked)_"
    else ""

    if mentions.isEmpty && unlinkedCount > 0 then
      s"*Interested:* _${unlinkedCount} users (none have linked Slack)_"
    else if mentions.isEmpty then
      "*Interested:* _No one yet_"
    else
      s"*Interested:* $linkedPart$unlinkedPart"

object SlackRosterService:
  val layer: ZLayer[SlackClient & SlackRosterMessageRepository & UserRepository, Nothing, SlackRosterService] =
    ZLayer.fromFunction((slackClient: SlackClient, rosterRepo: SlackRosterMessageRepository, userRepo: UserRepository) =>
      SlackRosterServiceLive(slackClient, rosterRepo, userRepo)
    )
