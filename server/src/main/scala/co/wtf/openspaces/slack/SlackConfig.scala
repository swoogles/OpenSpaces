package co.wtf.openspaces.slack

import zio.*

/** Initial config loaded from env vars. channelId is resolved at startup. */
case class SlackConfigEnv(
  botToken: String,
  channelName: String,
  appBaseUrl: String
)

/** Runtime config with resolved channel ID. */
case class SlackConfig(
  botToken: String,
  channelId: String,
  channelName: String,
  appBaseUrl: String
)

object SlackConfigEnv:
  private val DefaultChannelName = "openspaces-discussions-test"

  def fromEnv: UIO[Option[SlackConfigEnv]] =
    ZIO.succeed:
      for
        token   <- sys.env.get("SLACK_BOT_TOKEN")
        baseUrl <- sys.env.get("APP_BASE_URL")
        channelName = sys.env.getOrElse("SLACK_CHANNEL_NAME", DefaultChannelName)
      yield SlackConfigEnv(token, channelName, baseUrl)

  val layer: ZLayer[Any, Nothing, Option[SlackConfigEnv]] =
    ZLayer.fromZIO(fromEnv)
