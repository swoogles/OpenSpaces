package co.wtf.openspaces

import neotype.*
//import neotype.interop.ziojson.given
import zio.json.*

//  given codec: JsonCodec[Topic] =
//    JsonCodec.string.transformOrFail[Topic](s => Topic.make(s), _.unwrap)

case class TopicId(
  unwrap: Long)
object TopicId:
  given codec: JsonCodec[TopicId] =
    JsonCodec.long.transform[TopicId](s => TopicId(s), _.unwrap)

case class Topic(
  unwrap: String)
object Topic:
  def parse(
    raw: String,
  ) =
    Either.cond(
      raw.trim.length >= 10,
      Topic(raw),
      "Topic must be at least 10 characters long",
    )
  def parseOrDie(
    raw: String,
  ) =
    parse(raw).getOrElse(
      throw new Exception("Failed to parse topic: " + raw),
    )

  given codec: JsonCodec[Topic] =
    JsonCodec.string
      .transformOrFail[Topic](s => Topic.parse(s), _.unwrap)
