package co.wtf.openspaces.lighting_talks

import co.wtf.openspaces.Person
import neotype.*
import neotype.given
import neotype.interop.zioschema.given
import neotype.interop.ziojson.given
import zio.schema.*
import zio.json.*
import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

enum LightningTalkAction derives JsonCodec:
  case SetParticipation(
    speaker: Person,
    participating: Boolean)
  case SetAssignment(
    proposalId: LightningTalkId,
    expectedCurrentAssignment: Option[LightningAssignment],
    newAssignment: Option[LightningAssignment])
  case DrawForNextNight

enum LightningTalkActionConfirmed derives JsonCodec:
  case AddResult(
    proposal: LightningTalkProposal)
  case SlackThreadLinked(
    proposalId: LightningTalkId,
    slackThreadUrl: String)
  case Delete(
    proposalId: LightningTalkId,
    slackChannelId: Option[String],
    slackThreadTs: Option[String])
  case SetAssignment(
    proposalId: LightningTalkId,
    newAssignment: Option[LightningAssignment])
  case DrawForNightResult(
    night: LightningTalkNight,
    assignments: List[LightningDrawAssignment])
  case StateReplace(
    proposals: List[LightningTalkProposal])
  case Unauthorized(
    action: LightningTalkAction)
  case Rejected(
    action: LightningTalkAction)
