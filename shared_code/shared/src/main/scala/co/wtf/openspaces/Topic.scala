package co.wtf.openspaces

import neotype.*
import neotype.interop.ziojson.given
import zio.json.*

type TopicId = TopicId.Type
object TopicId extends Newtype[Long]

type Topic = Topic.Type
object Topic extends Newtype[String]:

  override inline def validate(
    value: String,
  ) =
    if value.nonEmpty && value.trim.length > 10 then true
    else "String must not be empty"
