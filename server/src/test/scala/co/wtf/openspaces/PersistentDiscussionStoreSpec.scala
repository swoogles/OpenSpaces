package co.wtf.openspaces

import co.wtf.openspaces.discussions.{Discussion, DiscussionAction, DiscussionActionConfirmed, DiscussionState, DiscussionStore}
import zio.*
import zio.test.*

/** Exercises the production discussion store against an in-memory database. */
object PersistentDiscussionStoreSpec extends ZIOSpecDefault:

  // Use the example state which has test slots
  private val slots = DiscussionState.example.slots

  private val sampleRoomSlot =
    val timeSlotForAllRooms = slots.head.slots.head
    RoomSlot(timeSlotForAllRooms.rooms.head, timeSlotForAllRooms.time)

  private def makeDataStore(
    state: DiscussionState,
  ): UIO[DiscussionStore] =
    ZIO.scoped(InMemoryDiscussionDb.storeLayer(state).build.map(_.get[DiscussionStore]))

  override def spec =
    suite("PersistentDiscussionStoreSpec")(
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
          before    <- dataStore.snapshot
          confirmed <- dataStore.applyAction(action)
          state     <- dataStore.snapshot
        } yield assertTrue(
          confirmed == DiscussionActionConfirmed.Rejected(action),
          state.data == before.data,
        )
      },
      test("concurrent bookings of the same slot: exactly one wins") {
        val attempts = (1 to 20).toList.map(i =>
          DiscussionAction.AddWithRoomSlot(Topic.unsafeMake(s"Concurrent topic $i"), Person(s"user$i"), sampleRoomSlot),
        )
        for {
          dataStore <- makeDataStore(DiscussionState(slots, Map.empty))
          results   <- ZIO.foreachPar(attempts)(dataStore.applyAction)
          state     <- dataStore.snapshot
        } yield assertTrue(
          results.count(_.isInstanceOf[DiscussionActionConfirmed.AddResult]) == 1,
          state.data.values.count(_.roomSlot.contains(sampleRoomSlot)) == 1,
        )
      } @@ TestAspect.withLiveRandom,
      test("Rename by the facilitator is applied") {
        val existing =
          Discussion(Topic.unsafeMake("Original title"), Person("Owner"), TopicId(7), GlyphiconUtils.names(0), roomSlot = None)
        val action = DiscussionAction.Rename(existing.id, Topic.unsafeMake("Better title"), Person("Owner"))
        for {
          dataStore <- makeDataStore(DiscussionState(slots, Map(existing.id -> existing)))
          confirmed <- dataStore.applyAction(action)
          state     <- dataStore.snapshot
        } yield assertTrue(
          confirmed == DiscussionActionConfirmed.Rename(existing.id, Topic.unsafeMake("Better title")),
          state.data(existing.id).topic == Topic.unsafeMake("Better title"),
        )
      },
      test("Rename by anyone other than the facilitator is Unauthorized") {
        val existing =
          Discussion(Topic.unsafeMake("Original title"), Person("Owner"), TopicId(8), GlyphiconUtils.names(0), roomSlot = None)
        val action = DiscussionAction.Rename(existing.id, Topic.unsafeMake("Hijacked title"), Person("Intruder"))
        for {
          dataStore <- makeDataStore(DiscussionState(slots, Map(existing.id -> existing)))
          confirmed <- dataStore.applyAction(action)
          state     <- dataStore.snapshot
        } yield assertTrue(
          confirmed == DiscussionActionConfirmed.Unauthorized(action),
          state.data(existing.id).topic == Topic.unsafeMake("Original title"),
        )
      },
      test("SetLockedTimeslot updates topic when scheduled and version matches") {
        val topic       = Topic.unsafeMake("Lock me")
        val facilitator = Person("Facilitator")
        val existingDiscussion =
          Discussion(
            topic,
            facilitator,
            TopicId(99),
            GlyphiconUtils.names(1),
            roomSlot = Some(sampleRoomSlot),
          )
        val discussionState =
          DiscussionState(
            slots,
            Map(existingDiscussion.id -> existingDiscussion),
          )
        val action = DiscussionAction.SetLockedTimeslot(
          existingDiscussion.id,
          expectedCurrentLockedTimeslot = false,
          newLockedTimeslot = true,
          requester = Person("admin"),
        )

        for {
          dataStore <- makeDataStore(discussionState)
          confirmed <- dataStore.applyAction(action)
          state     <- dataStore.snapshot
        } yield assertTrue(
          confirmed == DiscussionActionConfirmed.SetLockedTimeslot(
            existingDiscussion.id,
            lockedTimeslot = true,
          ),
          state.data(existingDiscussion.id).lockedTimeslot,
        )
      },
      test("SetLockedTimeslot is rejected when topic has no room slot") {
        val topic       = Topic.unsafeMake("Unsheduled topic")
        val facilitator = Person("Facilitator")
        val existingDiscussion =
          Discussion(
            topic,
            facilitator,
            TopicId(100),
            GlyphiconUtils.names(2),
            roomSlot = None,
          )
        val discussionState =
          DiscussionState(
            slots,
            Map(existingDiscussion.id -> existingDiscussion),
          )
        val action = DiscussionAction.SetLockedTimeslot(
          existingDiscussion.id,
          expectedCurrentLockedTimeslot = false,
          newLockedTimeslot = true,
          requester = Person("admin"),
        )

        for {
          dataStore <- makeDataStore(discussionState)
          confirmed <- dataStore.applyAction(action)
          state     <- dataStore.snapshot
        } yield assertTrue(
          confirmed == DiscussionActionConfirmed.Rejected(action),
          !state.data(existingDiscussion.id).lockedTimeslot,
        )
      },
    )
