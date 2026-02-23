package co.wtf.openspaces

import neotype.*
import neotype.interop.ziojson.given
import zio.json.*

// TODO Maybe rename "Content" ? Something like that, as I expect it to play the roles of:
// discussion title
// hackathon title
// meal plan
type TopicId = TopicId.Type
object TopicId extends Newtype[Long]

type Topic = Topic.Type
object Topic extends Newtype[String]:

  override inline def validate(
    value: String,
  ) =
    if (value.trim.length < 5)
      "Name must be at least 5 characters long"
    else if (value.trim.length > 100)
      "Name must be less than 100 characters long"
    else
      true
