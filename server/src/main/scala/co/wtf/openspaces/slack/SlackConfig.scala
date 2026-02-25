package co.wtf.openspaces.slack

import zio.*

/** Initial config loaded from env vars. Channel IDs are resolved at startup. */
case class SlackConfigEnv(
  botToken: String,
  channelName: String,
  hackathonChannelName: String,
  accessRequestChannelName: String,
  appBaseUrl: String
)

/** Runtime config with resolved channel IDs. */
case class SlackConfig(
  botToken: String,
  channelId: String,
  channelName: String,
  hackathonChannelId: String,
  hackathonChannelName: String,
  accessRequestChannelId: String,
  accessRequestChannelName: String,
  appBaseUrl: String
)

object SlackConfigEnv:
  private val DefaultChannelName = "openspaces-discussions-test"
  private val DefaultHackathonChannelName = "hackday-projects-test"
  private val DefaultAccessRequestChannelName = "access-requests-test"

  def fromEnv: UIO[Option[SlackConfigEnv]] =
    ZIO.succeed:
      for
        token   <- sys.env.get("SLACK_BOT_TOKEN")
        baseUrl <- sys.env.get("APP_BASE_URL")
        channelName = sys.env.getOrElse("SLACK_CHANNEL_NAME", DefaultChannelName)
        hackathonChannelName = sys.env.getOrElse("SLACK_HACKATHON_CHANNEL_NAME", DefaultHackathonChannelName)
        accessRequestChannelName = sys.env.getOrElse("SLACK_ACCESS_REQUEST_CHANNEL", DefaultAccessRequestChannelName)
      yield SlackConfigEnv(token, channelName, hackathonChannelName, accessRequestChannelName, baseUrl)

  val layer: ZLayer[Any, Nothing, Option[SlackConfigEnv]] =
    ZLayer.fromZIO(fromEnv)
