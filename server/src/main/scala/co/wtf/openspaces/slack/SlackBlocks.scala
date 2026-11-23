package co.wtf.openspaces.slack

import co.wtf.openspaces.RoomSlot
import zio.json.ast.Json

/** Builders for Slack Block Kit payloads.
  *
  * Blocks are built as a JSON AST, so user-provided text never needs manual
  * JSON escaping. Text that goes into mrkdwn still has Slack's control
  * characters (&, <, >) escaped via [[mrkdwn]].
  */
object SlackBlocks:

  /** Escape user text for Slack mrkdwn (https://api.slack.com/reference/surfaces/formatting#escaping). */
  def mrkdwn(text: String): String =
    text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

  /** Slack turns `:word:` into emoji; a ratio character (∶) avoids that in times like 10:40. */
  private def noEmoji(text: String): String = text.replace(":", "∶")

  private def avatarUrl(githubLogin: String): String =
    s"https://github.com/$githubLogin.png?size=100"

  private def text(value: String): Json =
    Json.Obj("type" -> Json.Str("mrkdwn"), "text" -> Json.Str(value))

  /** A headline section with the person's avatar, followed by a context line. */
  def card(
    headline: String,
    avatarLogin: String,
    altText: String,
    context: String,
  ): Json =
    Json.Arr(
      Json.Obj(
        "type"      -> Json.Str("section"),
        "text"      -> text(headline),
        "accessory" -> Json.Obj(
          "type"      -> Json.Str("image"),
          "image_url" -> Json.Str(avatarUrl(avatarLogin)),
          "alt_text"  -> Json.Str(altText),
        ),
      ),
      Json.Obj(
        "type"     -> Json.Str("context"),
        "elements" -> Json.Arr(text(context)),
      ),
    )

  private def appLink(appBaseUrl: String): String =
    s"<$appBaseUrl|View in OpenSpaces>"

  def discussion(
    topic: String,
    facilitatorLogin: String,
    facilitatorName: String,
    roomSlot: Option[RoomSlot],
    appBaseUrl: String,
  ): Json =
    val schedule = roomSlot.fold("")(rs =>
      s" · :round_pushpin: ${mrkdwn(rs.room.name)} · ${noEmoji(rs.timeSlot.displayString)}",
    )
    card(
      headline = s"*${mrkdwn(topic)}*",
      avatarLogin = facilitatorLogin,
      altText = facilitatorName,
      context = s"Proposed by *${mrkdwn(facilitatorName)}*$schedule · ${appLink(appBaseUrl)}",
    )

  def lightningTalk(
    speakerLogin: String,
    assignment: Option[(String, Int)],
    appBaseUrl: String,
  ): Json =
    val speaker = mrkdwn(speakerLogin)
    val assignmentInfo = assignment.fold("") { case (night, slot) => s" · :zap: $night Night slot #$slot" }
    card(
      headline = s":zap: *$speaker Lightning talk*",
      avatarLogin = speakerLogin,
      altText = speakerLogin,
      context = s"*$speaker* is willing to give a lightning talk$assignmentInfo · ${appLink(appBaseUrl)}",
    )

  def hackathonProject(
    title: String,
    ownerLogin: String,
    ownerName: String,
    appBaseUrl: String,
  ): Json =
    card(
      headline = s":hammer_and_wrench: *${mrkdwn(title)}*",
      avatarLogin = ownerLogin,
      altText = ownerName,
      context = s"Proposed by *${mrkdwn(ownerName)}* · ${appLink(appBaseUrl)}",
    )

  def activity(
    description: String,
    creatorLogin: String,
    creatorName: String,
    eventTime: Option[java.time.LocalDateTime],
    appBaseUrl: String,
  ): Json =
    val time = eventTime.fold("")(t => s" · ${t.toString.replace("T", " ")}")
    card(
      headline = s":calendar: *${mrkdwn(description)}*",
      avatarLogin = creatorLogin,
      altText = creatorName,
      context = s"Proposed by *${mrkdwn(creatorName)}*$time · ${appLink(appBaseUrl)}",
    )

  def accessRequest(
    username: String,
    displayName: Option[String],
    appBaseUrl: String,
  ): Json =
    val name = displayName.getOrElse(username)
    card(
      headline =
        s":wave: *New Access Request*\n*${mrkdwn(name)}* (@${mrkdwn(username)}) is requesting access to OpenSpaces.",
      avatarLogin = username,
      altText = name,
      context = s"<https://github.com/$username|GitHub Profile> · <$appBaseUrl|Open Admin Panel>",
    )
