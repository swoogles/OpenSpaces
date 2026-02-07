package co.wtf.openspaces.slack

import zio.*
import zio.http.*
import zio.json.*

case class SlackMessageRef(channel: String, ts: String)

trait SlackClient:
  def postMessage(channel: String, blocks: String): Task[SlackMessageRef]
  def updateMessage(channel: String, ts: String, blocks: String): Task[Unit]
  def postReply(channel: String, threadTs: String, text: String): Task[Unit]
  def getPermalink(channel: String, messageTs: String): Task[String]

class SlackClientLive(client: Client, config: SlackConfig) extends SlackClient:

  private val slackApiUrl = URL.decode("https://slack.com/api").getOrElse(throw new Exception("Bad Slack API URL"))

  private def slackRequest: Client =
    client
      .addHeader(Header.Authorization.Bearer(config.botToken))
      .addHeader(Header.ContentType(MediaType.application.json))
      .url(slackApiUrl)

  private def parseSlackResponse(body: String): Task[zio.json.ast.Json] =
    for
      json <- ZIO.fromEither(body.fromJson[zio.json.ast.Json]).mapError(e => new Exception(s"Failed to parse Slack response: $e"))
      ok = json match
        case zio.json.ast.Json.Obj(fields) => fields.collectFirst { case ("ok", zio.json.ast.Json.Bool(v)) => v }.getOrElse(false)
        case _ => false
      _ <- ZIO.when(!ok)(ZIO.fail(new Exception(s"Slack API error: $body")))
    yield json

  def postMessage(channel: String, blocks: String): Task[SlackMessageRef] =
    ZIO.scoped:
      for
        payload <- ZIO.succeed(s"""{"channel":"$channel","blocks":$blocks}""")
        res     <- slackRequest.post("/chat.postMessage")(Body.fromString(payload))
        body    <- res.body.asString
        json    <- parseSlackResponse(body)
        ref     <- extractMessageRef(json)
      yield ref

  def updateMessage(channel: String, ts: String, blocks: String): Task[Unit] =
    ZIO.scoped:
      for
        payload <- ZIO.succeed(s"""{"channel":"$channel","ts":"$ts","blocks":$blocks}""")
        res     <- slackRequest.post("/chat.update")(Body.fromString(payload))
        body    <- res.body.asString
        _       <- parseSlackResponse(body)
      yield ()

  def postReply(channel: String, threadTs: String, text: String): Task[Unit] =
    ZIO.scoped:
      for
        escapedText <- ZIO.succeed(text.replace("\"", "\\\""))
        payload     <- ZIO.succeed(s"""{"channel":"$channel","thread_ts":"$threadTs","text":"$escapedText"}""")
        res         <- slackRequest.post("/chat.postMessage")(Body.fromString(payload))
        body        <- res.body.asString
        _           <- parseSlackResponse(body)
      yield ()

  def getPermalink(channel: String, messageTs: String): Task[String] =
    ZIO.scoped:
      for
        res  <- slackRequest
                  .addQueryParam("channel", channel)
                  .addQueryParam("message_ts", messageTs)
                  .get("/chat.getPermalink")
        body <- res.body.asString
        json <- parseSlackResponse(body)
        permalink <- extractPermalink(json)
      yield permalink

  private def extractMessageRef(json: zio.json.ast.Json): Task[SlackMessageRef] =
    json match
      case zio.json.ast.Json.Obj(fields) =>
        val channel = fields.collectFirst { case ("channel", zio.json.ast.Json.Str(v)) => v }
        val ts = fields.collectFirst { case ("ts", zio.json.ast.Json.Str(v)) => v }
        (channel, ts) match
          case (Some(c), Some(t)) => ZIO.succeed(SlackMessageRef(c, t))
          case _ => ZIO.fail(new Exception(s"Missing channel/ts in Slack response: $json"))
      case _ => ZIO.fail(new Exception(s"Unexpected Slack response format: $json"))

  private def extractPermalink(json: zio.json.ast.Json): Task[String] =
    json match
      case zio.json.ast.Json.Obj(fields) =>
        fields.collectFirst { case ("permalink", zio.json.ast.Json.Str(v)) => v } match
          case Some(p) => ZIO.succeed(p)
          case None    => ZIO.fail(new Exception(s"Missing permalink in Slack response: $json"))
      case _ => ZIO.fail(new Exception(s"Unexpected Slack response format: $json"))
