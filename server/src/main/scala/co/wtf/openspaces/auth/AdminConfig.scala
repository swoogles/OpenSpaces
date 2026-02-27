package co.wtf.openspaces.auth

import zio.*

/** Admin configuration loaded from environment variables */
case class AdminConfig(
  adminUsers: Set[String],
  accessRequestChannelName: String
):
  def isAdmin(username: String): Boolean =
    adminUsers.contains(username.toLowerCase)

object AdminConfig:
  private val DefaultAccessRequestChannelName = "access-requests"

  def fromEnv: UIO[AdminConfig] =
    ZIO.succeed:
      val adminUsersRaw = sys.env.getOrElse("ADMIN_USERS", "swoogles")
      val adminUsers = adminUsersRaw
        .split(",")
        .map(_.trim.toLowerCase)
        .filter(_.nonEmpty)
        .toSet
      val accessRequestChannel = sys.env.getOrElse(
        "SLACK_ACCESS_REQUEST_CHANNEL_NAME",
        sys.env.getOrElse("SLACK_ACCESS_REQUEST_CHANNEL", DefaultAccessRequestChannelName),
      )
      AdminConfig(adminUsers, accessRequestChannel)

  val layer: ZLayer[Any, Nothing, AdminConfig] =
    ZLayer.fromZIO(fromEnv)
