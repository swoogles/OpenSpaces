package co.wtf.openspaces

import neotype.*
import neotype.interop.ziojson.given
import zio.json.*

type LightningTalkId = LightningTalkId.Type
object LightningTalkId extends Newtype[Long]

type LightningTalkSlot = LightningTalkSlot.Type
object LightningTalkSlot extends Newtype[Int]:
  override inline def validate(
    value: Int,
  ) =
    if value < 1 || value > 10 then
      "Lightning talk slot must be between 1 and 10"
    else
      true

enum LightningTalkNight derives JsonCodec:
  case Tuesday, Thursday

object LightningTalkNight:
  val ordered: List[LightningTalkNight] =
    List(Tuesday, Thursday)

case class LightningAssignment(
  night: LightningTalkNight,
  slot: LightningTalkSlot,
) derives JsonCodec

case class LightningTalkProposal(
  id: LightningTalkId,
  speaker: Person,
  speakerDisplayName: Option[String],
  assignment: Option[LightningAssignment],
  createdAtEpochMs: Long,
  slackThreadUrl: Option[String] = None,
) derives JsonCodec:
  val speakerName: String = speakerDisplayName.getOrElse(speaker.unwrap)

case class LightningDrawAssignment(
  proposalId: LightningTalkId,
  assignment: LightningAssignment,
) derives JsonCodec
