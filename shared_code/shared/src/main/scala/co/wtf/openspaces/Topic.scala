package co.wtf.openspaces

import neotype.*
//import neotype.interop.ziojson.given
import zio.json.*

type Topic = Topic.type
object Topic extends Newtype[String]
//  given codec: JsonCodec[Topic] =
//    JsonCodec.string.transformOrFail[Topic](s => Topic.make(s), _.unwrap)
