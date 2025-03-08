package co.wtf.openspaces

import co.wtf.openspaces.DiscussionAction.Rename

case class DiscussionState(
                            data: Map[TopicId, Discussion]
                          ):
  def apply(discussionAction: DiscussionAction): DiscussionState = {
    copy(data =
      discussionAction match
        case DiscussionAction.Delete(topicId) =>
          data.filterNot(_._2.id == topicId)
        case DiscussionAction.Add(discussion) =>
          // TODO Disallow duplicate names
          data + (discussion.id -> discussion) // Only add if new topic title
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