package co.wtf.openspaces

import co.wtf.openspaces.DiscussionAction.Rename

case class DiscussionState(
                            data: Map[TopicId, Discussion]
                          ):
  def apply(discussion: Discussion): DiscussionState = {
    copy(
      data = data + (discussion.id -> discussion) // Only add if new topic title
    )
  }
  def apply(discussionAction: DiscussionAction): DiscussionState = {
    copy(data =
      discussionAction match
        case DiscussionAction.Delete(topicId) =>
          data.filterNot(_._2.id == topicId)
        case DiscussionAction.Add(
          topic,
          facilitator,
        ) =>
          data // TOOD erm. // TODO Disallow duplicate names
        case DiscussionAction.Vote(topicId, voter) =>
          data.updatedWith(topicId) {
            _.map(value =>
              value.copy(interestedParties = value.interestedParties + voter))
          }
        case DiscussionAction.RemoveVote(topicId, voter) =>
          data.updatedWith(topicId) {
            _.map(value =>
              value.copy(interestedParties = value.interestedParties.filterNot(_.voter == voter)))
          }
        case Rename(topicId, newTopic) =>
          data.updatedWith(topicId) {
            _.map(value =>
              value.copy(topic = newTopic))
          }

        case DiscussionAction.AddResult(discussion) => data + (discussion.id -> discussion) // TODO Shouldn't actually be possible to submit this
    )
  }

object DiscussionState:
  def apply(input: Discussion*): DiscussionState =
    val startingState = Map(
      input.map(d => (d.id, d)) *
    )
    DiscussionState(
      startingState
    )