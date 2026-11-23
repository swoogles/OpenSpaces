package co.wtf.openspaces

import co.wtf.openspaces.discussions.*
import zio.json.*
import zio.test.*

object DiscussionActionTest extends ZIOSpecDefault:
  private val bill = Person("bill")
  private val topic = Topic.unsafeMake("test_topic")

  private val actions: List[DiscussionAction] = List(
    DiscussionAction.Add(topic, bill),
    DiscussionAction.Delete(TopicId(1L), bill),
    DiscussionAction.Vote(TopicId(1L), Feedback(bill, VotePosition.Interested)),
    DiscussionAction.Rename(TopicId(1L), topic, bill),
    DiscussionAction.SetLockedTimeslot(TopicId(1L), expectedCurrentLockedTimeslot = false, newLockedTimeslot = true, bill),
  )

  val spec =
    suite("DiscussionActionTest")(
      suite("json")(
        test("Add has the expected wire format") {
          val res = (DiscussionAction.Add(topic, bill): DiscussionAction).toJson
          assertTrue(res == """{"Add":{"topic":"test_topic","facilitator":"bill"}}""")
        },
        test("every action round-trips") {
          assertTrue(actions.forall(a => a.toJson.fromJson[DiscussionAction] == Right(a)))
        },
      ),
      suite("actor")(
        test("every action names the user performing it") {
          assertTrue(actions.forall(_.actor == bill))
        },
      ),
      suite("fromDiscussionAction")(
        test("SwapTopics confirms with the slots exchanged") {
          val slot1 = RoomSlot(Room.king, TimeSlot(java.time.LocalDateTime.parse("2026-03-03T09:00"), java.time.LocalDateTime.parse("2026-03-03T09:50")))
          val slot2 = RoomSlot(Room.hawk, slot1.timeSlot)
          val confirmed = DiscussionActionConfirmed.fromDiscussionAction(
            DiscussionAction.SwapTopics(TopicId(1L), slot1, TopicId(2L), slot2, bill),
          )
          assertTrue(confirmed == DiscussionActionConfirmed.SwapTopics(TopicId(1L), slot2, TopicId(2L), slot1))
        },
      ),
    )
