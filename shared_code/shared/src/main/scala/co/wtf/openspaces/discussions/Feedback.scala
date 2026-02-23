package co.wtf.openspaces.discussions

import co.wtf.openspaces.Person
import neotype.interop.ziojson.given
import zio.json.*

case class Feedback(
  voter: Person,
  position: VotePosition,
  firstVotedAtEpochMs: Option[Long] = None)
    derives JsonCodec