package co.wtf.openspaces

import co.wtf.openspaces.discussions.*
import co.wtf.openspaces.discussions.VotePosition.{Interested, NotInterested}
import zio.test.*

/** DiscussionState is the reducer both client and server use to apply confirmed events. */
object DiscussionStateTest extends ZIOSpecDefault:
  private val testUser1 = Person("testy-tester-1")
  private val testUser2 = Person("testy-tester-2")

  private val discussion =
    Discussion(
      Topic.unsafeMake("Continuous Deployment - A goal, or an asymptote?"),
      testUser1,
      TopicId(1),
      GlyphiconUtils.names(1),
      createdAtEpochMs = 1000L,
    )

  private val originalState = DiscussionState(List.empty, discussion)

  private def partiesOf(state: DiscussionState, id: TopicId = discussion.id): Set[Feedback] =
    state.data(id).interestedParties

  val spec =
    suite("DiscussionStateTest")(
      suite("Vote")(
        test("new vote is added") {
          val res = originalState(DiscussionActionConfirmed.Vote(discussion.id, Feedback(testUser2, Interested)))
          assertTrue(partiesOf(res) == Set(Feedback(testUser1, Interested), Feedback(testUser2, Interested)))
        },
        test("repeat vote by the same voter replaces their previous position") {
          val res = originalState(DiscussionActionConfirmed.Vote(discussion.id, Feedback(testUser1, NotInterested)))
          assertTrue(partiesOf(res) == Set(Feedback(testUser1, NotInterested)))
        },
        test("vote on an unknown topic changes nothing") {
          val res = originalState(DiscussionActionConfirmed.Vote(TopicId(99), Feedback(testUser2, Interested)))
          assertTrue(res == originalState)
        },
      ),
      suite("Add discussion")(
        test("new discussion is added") {
          val newDiscussion =
            Discussion(
              Topic.unsafeMake("Managing emotional energy on the job"),
              testUser2,
              TopicId(2),
              GlyphiconUtils.names(2),
              createdAtEpochMs = 2000L,
            )
          val res = originalState(DiscussionActionConfirmed.AddResult(newDiscussion))
          assertTrue(res.data == Map(discussion.id -> discussion, newDiscussion.id -> newDiscussion))
        },
        test("re-adding an existing discussion is idempotent") {
          val res = originalState(DiscussionActionConfirmed.AddResult(discussion))
          assertTrue(res == originalState)
        },
      ),
      suite("Remove discussion")(
        test("existing discussion is removed") {
          val res = originalState(DiscussionActionConfirmed.Delete(discussion.id))
          assertTrue(res.data.isEmpty)
        },
        test("removing a non-existent discussion changes nothing") {
          val res = originalState(DiscussionActionConfirmed.Delete(TopicId(2)))
          assertTrue(res == originalState)
        },
      ),
      suite("Facilitator changes")(
        test("old facilitator is recorded as NotInterested and the new one takes over") {
          val withSecondVoter =
            originalState(DiscussionActionConfirmed.Vote(discussion.id, Feedback(testUser2, Interested)))
          val res = withSecondVoter(
            DiscussionActionConfirmed.FacilitatorChanged(discussion.id, testUser2, Some("Tester Two")),
          )
          val updated = res.data(discussion.id)
          assertTrue(
            updated.facilitator == testUser2,
            updated.facilitatorDisplayName.contains("Tester Two"),
            updated.interestedParties == Set(Feedback(testUser1, NotInterested), Feedback(testUser2, Interested)),
          )
        },
        test("ResetUser deletes their topics and clears their other votes") {
          val otherTopic =
            Discussion(Topic.unsafeMake("Someone else's topic"), testUser2, TopicId(3), GlyphiconUtils.names(3), createdAtEpochMs = 3000L)
          val state =
            originalState(DiscussionActionConfirmed.AddResult(otherTopic))(
              DiscussionActionConfirmed.Vote(otherTopic.id, Feedback(testUser1, Interested)),
            )
          val res = state(DiscussionActionConfirmed.ResetUser(testUser1, List(discussion.id), List(otherTopic.id)))
          assertTrue(
            !res.data.contains(discussion.id),
            partiesOf(res, otherTopic.id) == Set(Feedback(testUser2, Interested)),
          )
        },
      ),
      suite("Scheduling")(
        test("SwapTopics exchanges room slots") {
          val slotA = RoomSlot(Room.king, TimeSlot(java.time.LocalDateTime.parse("2026-03-03T09:00"), java.time.LocalDateTime.parse("2026-03-03T09:50")))
          val slotB = RoomSlot(Room.hawk, slotA.timeSlot)
          val a = discussion.copy(roomSlot = Some(slotA))
          val b = Discussion(Topic.unsafeMake("Second scheduled topic"), testUser2, TopicId(4), GlyphiconUtils.names(4), roomSlot = Some(slotB), createdAtEpochMs = 4000L)
          val state = DiscussionState(List.empty, a, b)
          val res = state(DiscussionActionConfirmed.SwapTopics(a.id, slotB, b.id, slotA))
          assertTrue(
            res.data(a.id).roomSlot.contains(slotB),
            res.data(b.id).roomSlot.contains(slotA),
          )
        },
      ),
    )
