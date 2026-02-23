package co.wtf.openspaces.discussions

import co.wtf.openspaces.{Glyphicon, GlyphiconUtils, Person, Topic, TopicId, Room, RoomSlot, TimeSlot}
import co.wtf.openspaces.discussions.VotePosition.Interested
import neotype.unwrap
import neotype.interop.ziojson.given
import zio.json.*
import java.time.LocalDateTime

case class Discussion(
  topic: Topic,
  facilitator: Person,
  interestedParties: Set[Feedback],
  id: TopicId,
  glyphicon: Glyphicon,
  roomSlot: Option[RoomSlot],
  lockedTimeslot: Boolean,
  facilitatorDisplayName: Option[String],
  slackThreadUrl: Option[String],
  createdAtEpochMs: Long)
    derives JsonCodec:

  val votes: Int = interestedParties.count(_.position == Interested)
  val facilitatorName = facilitatorDisplayName.getOrElse(facilitator.unwrap)
  val topicName = topic.unwrap

object Discussion:
  def apply(
    topic: Topic,
    facilitator: Person,
    id: TopicId,
    glyphicon: Glyphicon,
    roomSlot: Option[RoomSlot] = None,
    facilitatorDisplayName: Option[String] = None,
    createdAtEpochMs: Long = java.lang.System.currentTimeMillis(),
  ): Discussion =
    Discussion(
      topic,
      facilitator,
      Set(Feedback(facilitator, Interested)),
      id,
      glyphicon,
      roomSlot,
      lockedTimeslot = false,
      facilitatorDisplayName,
      None,
      createdAtEpochMs,
    )

  val example1 = Discussion(
    Topic(
      "Continuous Deployment - A goal, an asymptote, or an ass out of you and me?",
    ),
    Person("Bill"),
    TopicId(1),
    GlyphiconUtils.names(1),
    roomSlot = Some(
      RoomSlot(Room.king,
               TimeSlot("9:00-9:50",
                        LocalDateTime.parse("2026-03-03T09:00:00"),
                        LocalDateTime.parse("2026-03-03T09:50:00"),
               ),
      ),
    ),
  )

  val example2 = Discussion(
    Topic("Managing emotional energy on the job"),
    Person("Emma"),
    TopicId(2),
    GlyphiconUtils.names(2),
    Some(
      RoomSlot(Room.artGallery,
               TimeSlot("9:00-9:50",
                        LocalDateTime.parse("2026-03-03T09:00:00"),
                        LocalDateTime.parse("2026-03-03T09:50:00"),
               ),
      ),
    ),
  )

  val example3 =
    Discussion(
      Topic("How to make a great cup of coffee"),
      Person("John"),
      TopicId(3),
      GlyphiconUtils.names(3),
      Some(
        RoomSlot(Room.king,
                 TimeSlot("10:20-11:10",
                          LocalDateTime.parse("2026-03-03T10:20:00"),
                          LocalDateTime.parse("2026-03-03T11:10:00"),
                 ),
        ),
      ),
    )

  val example4 =
    Discussion(
      Topic("How to make a great cup of tea"),
      Person("John"),
      Set(Feedback(Person("Bill"), VotePosition.NotInterested)),
      TopicId(4),
      GlyphiconUtils.names(4),
      None,
      false,
      None,
      None,
      0L,
    )

  val example5 =
    Discussion(
      Topic("Fighting round the world"),
      Person("John"),
      Set.empty,
      TopicId(5),
      GlyphiconUtils.names(5),
      None,
      false,
      None,
      None,
      0L,
    )

  val example6 =
    Discussion(
      Topic("Pandas - friends or foes?"),
      Person("John"),
      Set.empty,
      TopicId(5),
      GlyphiconUtils.names(6),
      None,
      false,
      None,
      None,
      0L,
    )
