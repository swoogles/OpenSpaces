package co.wtf.openspaces

import co.wtf.openspaces.discussions.DiscussionAction
import zio._
import zio.test._
import zio.json._

object DiscussionActionTest extends ZIOSpecDefault:
  val spec =
    suite("DiscussionActionTest")(
      suite("json")(
        test("add"):
          val discussion = Discussion(
            Topic.parseOrDie("test_topic"),
            facilitator = Person("bill"),
            interestedParties = Set(Person("bill")),
            TopicId(0L),
          )
          val res = DiscussionAction
            .Add(discussion)
            .asInstanceOf[DiscussionAction]
            .toJsonPretty
          val expected =
            """{
               |  "Add" : {
               |    "discussion" : {
               |      "topic" : "test_topic",
               |      "facilitator" : "bill",
               |      "interestedParties" : [
               |        "bill"
               |      ],
               |      "id" : 0
               |    }
               |  }
               |}""".stripMargin
          assertTrue(res == expected),
      ),
    )
