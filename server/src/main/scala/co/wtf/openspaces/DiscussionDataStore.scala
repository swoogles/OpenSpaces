package co.wtf.openspaces

import zio.*
import zio.direct.*

class DiscussionDataStore(discussionDatabase: Ref[DiscussionState],
                          glyphiconService: GlyphiconService):
  def snapshot =
    discussionDatabase.get

  def applyAction(discussionAction: DiscussionAction): UIO[DiscussionActionConfirmed] =
    discussionAction match
      case DiscussionAction.Add(topic, facilitator) =>
        for {
          randomIcon <- glyphiconService.getRandomIcon
          randomId <- Random.nextLong
          discussion = Discussion(
            topic,
            facilitator,
            Set(Feedback(facilitator, VotePosition.Interested)),
            TopicId(randomId),
            randomIcon,
            None
          )
          res <- discussionDatabase.updateAndGet(s => s(discussion))
        } yield DiscussionActionConfirmed.AddResult(discussion)

      case other =>
        defer:
          val confirmedAction = DiscussionActionConfirmed.fromDiscussionAction(other)
          discussionDatabase.updateAndGet(s => s(confirmedAction)).run
          confirmedAction

  private def randomExistingTopicId =
    defer:
      val data = snapshot.run
      val idx = Random.nextIntBounded(data.data.keys.toList.length).run
      data.data.keys.toList(idx)


  def randomDiscussionAction =
    defer:
      val actionIdx = Random.nextIntBounded(5).run
      val noCurrentItems = snapshot.run.data.keys.toList.isEmpty
      val addNewDiscussion =
        val person = Person("RandomPerson - " + Random.nextIntBounded(20).run)
        DiscussionAction.Add(
          DiscussionTopics.randomTopic.run,
          person,
        )

      val action =
        if (noCurrentItems)
          addNewDiscussion
        else
          actionIdx match {
            case 0 =>
              addNewDiscussion
            case 1 =>
              val id = randomExistingTopicId.run
              DiscussionAction.Delete(id)
            case 2 =>
              val id = randomExistingTopicId.run
              val person = Person("RandomPerson - " + Random.nextIntBounded(20).run)
              DiscussionAction.Vote(id, Feedback(person, VotePosition.Interested))

            case 3 =>
              val id = randomExistingTopicId.run
              // TODO Ensure existing person that has voted for topic, unless it's got 0 votes
              val person = Person("RandomPerson - " + Random.nextIntBounded(20).run)
              DiscussionAction.RemoveVote(id, person)
            case 4 =>
              val id = randomExistingTopicId.run
              val newTopic = DiscussionTopics.randomTopic.run
              DiscussionAction.Rename(id, newTopic)
          }
      applyAction(action).as(action).run

object DiscussionDataStore:
  val layer =
    ZLayer.fromZIO:
      defer:
        DiscussionDataStore(
          Ref.make(
            DiscussionState.example
          ).run,
          ZIO.service[GlyphiconService].run
        )
