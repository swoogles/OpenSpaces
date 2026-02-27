package co.wtf.openspaces.slack

import zio.*

/** Initial config loaded from env vars. Channel IDs are resolved at startup. */
case class SlackConfigEnv(
  botToken: String,
  channelName: String,
  lightningChannelName: String,
  activityChannelName: String,
  hackathonChannelName: String,
  accessRequestChannelName: String,
  appBaseUrl: String
)

/** Runtime config with resolved channel IDs. */
case class SlackConfig(
  botToken: String,
  channelId: String,
  channelName: String,
  lightningChannelId: String,
  lightningChannelName: String,
  activityChannelId: String,
  activityChannelName: String,
  hackathonChannelId: String,
  hackathonChannelName: String,
  accessRequestChannelId: String,
  accessRequestChannelName: String,
  appBaseUrl: String
)

object SlackConfigEnv:
  private val DefaultChannelName = "openspaces-discussions"
  private val DefaultLightningChannelName = "lightning-talks"
  private val DefaultActivityChannelName = "activities"
  private val DefaultHackathonChannelName = "hackday-projects"
  private val DefaultAccessRequestChannelName = "access-requests"

  def fromEnv: UIO[Option[SlackConfigEnv]] =
    ZIO.succeed:
      for
        token   <- sys.env.get("SLACK_BOT_TOKEN")
        baseUrl <- sys.env.get("APP_BASE_URL")
        channelName = sys.env.getOrElse("SLACK_CHANNEL_NAME", DefaultChannelName)
        lightningChannelName = sys.env.getOrElse("SLACK_LIGHTNING_CHANNEL_NAME", DefaultLightningChannelName)
        activityChannelName = sys.env.getOrElse("SLACK_ACTIVITY_CHANNEL_NAME", DefaultActivityChannelName)
        hackathonChannelName = sys.env.getOrElse("SLACK_HACKATHON_CHANNEL_NAME", DefaultHackathonChannelName)
        accessRequestChannelName = sys.env
          .get("SLACK_ACCESS_REQUEST_CHANNEL_NAME")
          .orElse(sys.env.get("SLACK_ACCESS_REQUEST_CHANNEL"))
          .getOrElse(DefaultAccessRequestChannelName)
      yield SlackConfigEnv(
        token,
        channelName,
        lightningChannelName,
        activityChannelName,
        hackathonChannelName,
        accessRequestChannelName,
        baseUrl,
      )

  val layer: ZLayer[Any, Nothing, Option[SlackConfigEnv]] =
    ZLayer.fromZIO(fromEnv)
