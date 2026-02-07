package co.wtf.openspaces.slack

import zio.*

case class SlackConfig(
  botToken: String,
  channelId: String,
  appBaseUrl: String
)

object SlackConfig:
  def fromEnv: UIO[Option[SlackConfig]] =
    ZIO.succeed:
      for
        token   <- sys.env.get("SLACK_BOT_TOKEN")
        channel <- sys.env.get("SLACK_CHANNEL_ID")
        baseUrl <- sys.env.get("APP_BASE_URL")
      yield SlackConfig(token, channel, baseUrl)

  val layer: ZLayer[Any, Nothing, Option[SlackConfig]] =
    ZLayer.fromZIO(fromEnv)
