package co.wtf.openspaces

import co.wtf.openspaces.slack.SlackBlocks
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object SlackBlocksSpec extends ZIOSpecDefault:

  private val trickyTitle = "Say \"hi\" \\ to <everyone> & \nnew lines"

  private def headline(blocks: Json): Option[String] =
    blocks match
      case Json.Arr(elements) =>
        elements.headOption.collect { case Json.Obj(fields) => fields.toMap }
          .flatMap(_.get("text"))
          .collect { case Json.Obj(fields) => fields.toMap }
          .flatMap(_.get("text"))
          .collect { case Json.Str(value) => value }
      case _ => None

  override def spec =
    suite("SlackBlocks")(
      test("user text with quotes, backslashes and newlines still produces valid JSON") {
        val blocks = SlackBlocks.hackathonProject(trickyTitle, "octocat", "The \"Octo\" Cat", "https://example.com")
        val roundTripped = blocks.toJson.fromJson[Json]
        assertTrue(roundTripped == Right(blocks))
      },
      test("Slack mrkdwn control characters are escaped") {
        val blocks = SlackBlocks.discussion(trickyTitle, "octocat", "Octo", None, "https://example.com")
        assertTrue(
          headline(blocks).contains("*Say \"hi\" \\ to &lt;everyone&gt; &amp; \nnew lines*"),
        )
      },
    )
