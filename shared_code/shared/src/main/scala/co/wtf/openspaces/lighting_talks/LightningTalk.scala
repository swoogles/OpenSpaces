package co.wtf.openspaces.lighting_talks

import co.wtf.openspaces.Person
import neotype.*
import neotype.interop.ziojson.given
import zio.json.*
import java.time.LocalDate

type LightningTalkId = LightningTalkId.Type
object LightningTalkId extends Newtype[Long]

type LightningTalkSlot = LightningTalkSlot.Type
object LightningTalkSlot extends Newtype[Int]:
  override inline def validate(
    value: Int,
  ) =
    if value < 1 || value > 10 then
      "Lightning talk slot must be between 1 and 10"
    else true

enum LightningTalkNight derives JsonCodec:
  case Tuesday, Thursday

object LightningTalkNight:
  val ordered: List[LightningTalkNight] =
    List(Tuesday, Thursday)

  def dateFor(
    night: LightningTalkNight,
  ): LocalDate =
    night match
      case Tuesday  => LocalDate.of(2026, 3, 3)
      case Thursday => LocalDate.of(2026, 3, 5)

  def futureNights(
    today: LocalDate,
  ): List[LightningTalkNight] =
    ordered.filter(night => !dateFor(night).isBefore(today))

case class LightningAssignment(
  night: LightningTalkNight,
  slot: LightningTalkSlot)
    derives JsonCodec

case class LightningTalkProposal(
  id: LightningTalkId,
  speaker: Person,
  speakerDisplayName: Option[String],
  assignment: Option[LightningAssignment],
  createdAtEpochMs: Long,
  slackThreadUrl: Option[String] = None)
    derives JsonCodec:
  val speakerName: String =
    speakerDisplayName.getOrElse(speaker.unwrap)

case class LightningDrawAssignment(
  proposalId: LightningTalkId,
  assignment: LightningAssignment)
    derives JsonCodec
