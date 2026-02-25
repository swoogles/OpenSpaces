package co.wtf.openspaces.activities

import co.wtf.openspaces.Person
import neotype.*
import neotype.unwrap
import neotype.interop.ziojson.given
import zio.json.*
import java.time.LocalDateTime

type ActivityId = ActivityId.Type
object ActivityId extends Newtype[Long]

type ActivityDescription = ActivityDescription.Type
object ActivityDescription extends Newtype[String]:
  override inline def validate(value: String) =
    val trimmed = value.trim
    if trimmed.length < 3 then "Activity description must be at least 3 characters"
    else if trimmed.length > 120 then "Activity description must be 120 characters or less"
    else true

case class Activity(
  id: ActivityId,
  description: ActivityDescription,
  creator: Person,
  creatorDisplayName: Option[String],
  eventTime: LocalDateTime,
  interestedPeople: Set[Person],
  createdAtEpochMs: Long,
  slackThreadUrl: Option[String] = None,
) derives JsonCodec:
  val descriptionText: String = description.unwrap
  val creatorName: String = creatorDisplayName.getOrElse(creator.unwrap)
  val interestCount: Int = interestedPeople.size
