package co.wtf.openspaces

import co.wtf.openspaces.discussions.{Discussion, DiscussionAction, DiscussionActionConfirmed, DiscussionDataStore, DiscussionState}
import zio.*
import zio.test.*
import zio.test.TestRandom

object DiscussionDataStoreSpec extends ZIOSpecDefault:

  private val slots = DiscussionState.timeSlotExamples

  private val sampleRoomSlot =
    val timeSlotForAllRooms = slots.head.slots.head
    RoomSlot(timeSlotForAllRooms.rooms.head, timeSlotForAllRooms.time)

  private def makeDataStore(
    state: DiscussionState,
  ): UIO[DiscussionDataStore] =
    for {
      ref          <- Ref.make(state)
      glyphService <- GlyphiconService.make
    } yield DiscussionDataStore(ref, glyphService)

  override def spec =
    suite("DiscussionDataStoreSpec")(
      test("AddWithRoomSlot stores discussion when slot free") {
        val topic        = Topic.unsafeMake("Topic scheduled at creation")
        val facilitator  = Person("Facilitator")
        val discussionState =
          DiscussionState(slots, Map.empty)
        val action = DiscussionAction.AddWithRoomSlot(topic,
                                                      facilitator,
                                                      sampleRoomSlot)
        for {
          dataStore <- makeDataStore(discussionState)
          _         <- TestRandom.feedInts(0)
          _         <- TestRandom.feedLongs(1L)
          confirmed <- dataStore.applyAction(action)
          state     <- dataStore.snapshot
          discussion = confirmed match
            case DiscussionActionConfirmed.AddResult(value) => value
            case other =>
              throw new RuntimeException(
                s"Expected AddResult but got $other",
              )
        } yield assertTrue(
          discussion.roomSlot.contains(sampleRoomSlot),
          state.data.contains(discussion.id),
          state.data(discussion.id).roomSlot.contains(sampleRoomSlot),
        )
      },
      test("AddWithRoomSlot is rejected when slot occupied") {
        val topic       = Topic.unsafeMake("Existing Topic")
        val facilitator = Person("Existing Facilitator")
        val existingDiscussion =
          Discussion(topic,
                     facilitator,
                     TopicId(42),
                     GlyphiconUtils.names(0),
                     roomSlot = Some(sampleRoomSlot),
          )
        val discussionState =
          DiscussionState(
            slots,
            Map(existingDiscussion.id -> existingDiscussion),
          )
        val action = DiscussionAction.AddWithRoomSlot(
          Topic.unsafeMake("Conflicting Topic"),
          Person("New Facilitator"),
          sampleRoomSlot,
        )
        for {
          dataStore <- makeDataStore(discussionState)
          confirmed <- dataStore.applyAction(action)
          state     <- dataStore.snapshot
        } yield assertTrue(
          confirmed == DiscussionActionConfirmed.Rejected(action),
          state.data == discussionState.data,
        )
      },
    )
