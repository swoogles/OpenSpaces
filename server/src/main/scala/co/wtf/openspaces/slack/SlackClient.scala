package co.wtf.openspaces.slack

import zio.*
import zio.http.*
import zio.json.*

case class SlackMessageRef(channel: String, ts: String)

trait SlackClient:
  def postMessage(channel: String, blocks: String): Task[SlackMessageRef]
  def updateMessage(channel: String, ts: String, blocks: String): Task[Unit]
  def deleteMessage(channel: String, ts: String): Task[Unit]
  def postReply(channel: String, threadTs: String, text: String): Task[Unit]
  def getPermalink(channel: String, messageTs: String): Task[String]
  def findChannelByName(name: String): Task[Option[String]]
  def createChannel(name: String): Task[String]

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

  def deleteMessage(channel: String, ts: String): Task[Unit] =
    ZIO.scoped:
      for
        payload <- ZIO.succeed(s"""{"channel":"$channel","ts":"$ts"}""")
        res     <- slackRequest.post("/chat.delete")(Body.fromString(payload))
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
        // Append thread params so the link opens the thread expanded in reply mode
        threadUrl = s"$permalink?thread_ts=$messageTs&cid=$channel"
      yield threadUrl

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

  def findChannelByName(name: String): Task[Option[String]] =
    ZIO.scoped:
      for
        res  <- slackRequest
                  .addQueryParam("types", "public_channel")
                  .addQueryParam("limit", "1000")
                  .get("/conversations.list")
        body <- res.body.asString
        json <- parseSlackResponse(body)
        channelId = extractChannelIdByName(json, name)
      yield channelId

  def createChannel(name: String): Task[String] =
    ZIO.scoped:
      for
        payload <- ZIO.succeed(s"""{"name":"$name"}""")
        res     <- slackRequest.post("/conversations.create")(Body.fromString(payload))
        body    <- res.body.asString
        json    <- parseSlackResponse(body)
        channelId <- extractCreatedChannelId(json)
      yield channelId

  private def extractChannelIdByName(json: zio.json.ast.Json, targetName: String): Option[String] =
    json match
      case zio.json.ast.Json.Obj(fields) =>
        fields.collectFirst { case ("channels", zio.json.ast.Json.Arr(channels)) => channels }.flatMap { channels =>
          channels.collectFirst {
            case zio.json.ast.Json.Obj(chFields) 
              if chFields.collectFirst { case ("name", zio.json.ast.Json.Str(n)) => n }.contains(targetName) =>
              chFields.collectFirst { case ("id", zio.json.ast.Json.Str(id)) => id }
          }.flatten
        }
      case _ => None

  private def extractCreatedChannelId(json: zio.json.ast.Json): Task[String] =
    json match
      case zio.json.ast.Json.Obj(fields) =>
        fields.collectFirst { case ("channel", zio.json.ast.Json.Obj(chFields)) => chFields }.flatMap { chFields =>
          chFields.collectFirst { case ("id", zio.json.ast.Json.Str(id)) => id }
        } match
          case Some(id) => ZIO.succeed(id)
          case None     => ZIO.fail(new Exception(s"Missing channel.id in Slack response: $json"))
      case _ => ZIO.fail(new Exception(s"Unexpected Slack response format: $json"))
