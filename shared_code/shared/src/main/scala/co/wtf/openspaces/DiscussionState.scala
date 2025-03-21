package co.wtf.openspaces

case class DiscussionState(
                            slots: List[TimeSlotForAllRooms],
                            data: Map[TopicId, Discussion],
                          ):
  def roomSlotContent(roomSlot: RoomSlot): Option[Discussion] =
    println("Looking in roomSlot: " + roomSlot)
    data.values.find(_.roomSlot.contains(roomSlot))

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
        case DiscussionAction.Rename(topicId, newTopic) =>
          data.updatedWith(topicId) {
            _.map(value =>
              value.copy(topic = newTopic))
          }
    )
  }


  def apply(discussionAction: DiscussionActionConfirmed): DiscussionState = {
    copy(data =
      discussionAction match
        case DiscussionActionConfirmed.Delete(topicId) =>
          data.filterNot(_._2.id == topicId)
        case DiscussionActionConfirmed.Vote(topicId, voter) =>
          data.updatedWith(topicId) {
            _.map(value =>
              value.copy(interestedParties = value.interestedParties + voter))
          }
        case DiscussionActionConfirmed.RemoveVote(topicId, voter) =>
          data.updatedWith(topicId) {
            _.map(value =>
              value.copy(interestedParties = value.interestedParties.filterNot(_.voter == voter)))
          }
        case DiscussionActionConfirmed.Rename(topicId, newTopic) =>
          data.updatedWith(topicId) {
            _.map(value =>
              value.copy(topic = newTopic))
          }

        case DiscussionActionConfirmed.AddResult(discussion) => data + (discussion.id -> discussion)
    )
  }

object DiscussionState:
  def apply(
             slots: List[TimeSlotForAllRooms],
             input: Discussion*): DiscussionState =
    val startingState = Map(
      input.map(d => (d.id, d)) *
    )


    DiscussionState(
      slots, // Instead of prepoulating slots here, it should be derived from the discussions, namely discussion.roomSlot
      startingState,
    )

  val timeSlotExamples =
    List(
      TimeSlotForAllRooms(
        TimeSlot("8:00-8:50"),
        List(Room.king, Room.hawk, Room.artGallery, Room.danceHall)
      ),
      TimeSlotForAllRooms(
        TimeSlot("9:20-10:10"),
        List(Room.king, Room.hawk, Room.artGallery, Room.danceHall)
      )
    )

  val example =
    DiscussionState(
      timeSlotExamples,
      Discussion.example1,
      Discussion.example2,
      Discussion.example3,
      Discussion.example4,
      Discussion.example5,
      Discussion.example6
    )