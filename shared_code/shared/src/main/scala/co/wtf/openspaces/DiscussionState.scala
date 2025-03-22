package co.wtf.openspaces

case class DiscussionState(
                            slots: List[TimeSlotForAllRooms],
                            data: Map[TopicId, Discussion],
                          ):
  def roomSlotContent(roomSlot: RoomSlot): Option[Discussion] =
    data.values.find(_.roomSlot.contains(roomSlot))

  def apply(discussion: Discussion): DiscussionState = {
    copy(
      data = data + (discussion.id -> discussion) // Only add if new topic title
    )
  }

  def apply(discussionAction: DiscussionActionConfirmed): DiscussionState = {
    copy(data =
      discussionAction match
        case DiscussionActionConfirmed.UpdateRoomSlot(topicId, roomSlot) =>
          data.updatedWith(topicId) {
            _.map(value =>
              value.copy(roomSlot = Some(roomSlot)))
          }
        case DiscussionActionConfirmed.Delete(topicId) =>
          val beforeDelete = data
          val res = data.filterNot(_._2.id == topicId)
          println("beforeDelete: " + beforeDelete.size)
          println("res: " + res.size)
          res
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