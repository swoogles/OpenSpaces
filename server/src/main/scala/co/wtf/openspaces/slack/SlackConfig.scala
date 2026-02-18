package co.wtf.openspaces.slack

import zio.*

/** Initial config loaded from env vars. Channel IDs are resolved at startup. */
case class SlackConfigEnv(
  botToken: String,
  channelName: String,
  hackathonChannelName: String,
  appBaseUrl: String
)

/** Runtime config with resolved channel IDs. */
case class SlackConfig(
  botToken: String,
  channelId: String,
  channelName: String,
  hackathonChannelId: String,
  hackathonChannelName: String,
  appBaseUrl: String
)

object SlackConfigEnv:
  private val DefaultChannelName = "openspaces-discussions-test"
  private val DefaultHackathonChannelName = "hackday-projects-test"

  def fromEnv: UIO[Option[SlackConfigEnv]] =
    ZIO.succeed:
      for
        token   <- sys.env.get("SLACK_BOT_TOKEN")
        baseUrl <- sys.env.get("APP_BASE_URL")
        channelName = sys.env.getOrElse("SLACK_CHANNEL_NAME", DefaultChannelName)
        hackathonChannelName = sys.env.getOrElse("SLACK_HACKATHON_CHANNEL_NAME", DefaultHackathonChannelName)
      yield SlackConfigEnv(token, channelName, hackathonChannelName, baseUrl)

  val layer: ZLayer[Any, Nothing, Option[SlackConfigEnv]] =
    ZLayer.fromZIO(fromEnv)
