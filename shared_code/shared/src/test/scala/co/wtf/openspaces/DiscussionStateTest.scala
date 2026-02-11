package co.wtf.openspaces

import zio.*
import zio.json.*
import zio.test.*

object DiscussionStateTest extends ZIOSpecDefault:
  val testUser1 = Person("testy-tester-1")
  val testUser2 = Person("testy-tester-2")
  val discussion =
    Discussion(
      Topic.parseOrDie(
        "Continuous Deployment - A goal, or an asymptote?",
      ),
      testUser1,
      Set(testUser1),
      TopicId(1),
    )
  val originalState =
    DiscussionState(
      discussion,
    )
  val spec =
    suite("DiscussionStateTest")(
      suite("Vote")(
        test("New vote"):
          val res =
            originalState(
              DiscussionAction.Vote(discussion.id, testUser2),
            )
          val expected = DiscussionState(
            discussion.copy(
              interestedParties = Set(
                testUser1,
                testUser2,
              ),
            ),
          )
          assertTrue(res == expected)
        ,
        test("Existing vote"):
          val res =
            originalState(
              DiscussionAction.Vote(discussion.id, testUser1),
            )
          val expected = DiscussionState(
            discussion.copy(
              interestedParties = Set(
                testUser1,
              ),
            ),
          )
          assertTrue(res == expected),
      ),
      suite("Add discussion")(
        test("new discussion"):
          val newDiscussion =
            Discussion(
              Topic.parseOrDie(
                "Managing emotional energy on the job",
              ),
              testUser2,
              Set(Feedback(testUser2, VotePosition.Interested)),
              TopicId(2),
              GlyphiconUtils.names(2),
            )
          val res =
            originalState(
              DiscussionAction.Add(newDiscussion),
            )
          val expected = DiscussionState(
            Map(
              discussion.id    -> discussion,
              newDiscussion.id -> newDiscussion,
            ),
          )
          assertTrue(res == expected)
        ,
        test("existing discussion"):
          val res =
            originalState(
              DiscussionAction.Add(discussion),
            )
          val expected = DiscussionState(
            Map(
              discussion.id -> discussion,
            ),
          )
          assertTrue(res == expected),
      ),
      suite("Remove discussion")(
        test("existing discussion"):
          val res =
            originalState(
              DiscussionAction.Delete(discussion.id),
            )
          val expected = DiscussionState(
            Map(),
          )
          assertTrue(res == expected)
        ,
        test("non-existent discussion"):
          val res =
            originalState(
              DiscussionAction.Delete(TopicId(2)),
            )
          val expected = DiscussionState(
            Map(
              discussion.id -> discussion,
            ),
          )
          assertTrue(res == expected),
      ),
    )
