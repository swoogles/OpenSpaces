package co.wtf.openspaces

import zio.*
import zio.json.*
import zio.test.*

object DiscussionStateTest extends ZIOSpecDefault:
  val testUser1 = "testy-tester-1"
  val testUser2 = "testy-tester-2"
  val discussion =
    Discussion(
      Topic.parseOrDie("Continuous Deployment - A goal, or an asymptote?"),
      testUser1,
      Set(testUser1),
      TopicId(1)
    )
  val originalState =
    DiscussionState(
    discussion
  )
  val spec =
    suite("DiscussionStateTest")(
      suite("apply action")(
        suite("Vote")(
          test("New vote"):
            val res =
              originalState(
                DiscussionAction.Vote(discussion.id, testUser2)
              )
            val expected = DiscussionState(
              discussion.copy(
                interestedParties =
                  Set(
                    testUser1,
                    testUser2
                  )
              )
            )
            assertTrue(res == expected)
          ,
          test("Existing vote"):
            val res =
              originalState(
                DiscussionAction.Vote(discussion.id, testUser1)
              )
            val expected = DiscussionState(
              discussion.copy(
                interestedParties =
                  Set(
                    testUser1,
                  )
              )
            )
            assertTrue(res == expected)
        ),
        suite("RemoveVote")(
          test("Remove vote"):
            val res =
              originalState(
                DiscussionAction.RemoveVote(discussion.id, testUser1)
              )
            val expected = DiscussionState(
              discussion.copy(
                interestedParties =
                  Set()
              )
            )
            assertTrue(res == expected)
          ,
          test("Remove non-existent vote"):
            val res =
              originalState(
                DiscussionAction.RemoveVote(discussion.id, testUser2)
              )
            val expected = DiscussionState(
              discussion.copy(
                interestedParties =
                  Set(testUser1)
              )
            )
            assertTrue(res == expected)
        )
      )
    )
