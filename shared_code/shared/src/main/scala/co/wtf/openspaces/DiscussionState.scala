package co.wtf.openspaces

import co.wtf.openspaces.DiscussionActionConfirmed.Rejected

import java.time.LocalDate
import java.time.LocalDateTime

case class DiscussionState(
  slots: List[DaySlots],
  data: Map[TopicId, Discussion]):
  def roomSlotContent(
    roomSlot: RoomSlot,
  ): Option[Discussion] =
    data.values.find(_.roomSlot.contains(roomSlot))

  def apply(
    discussion: Discussion,
  ): DiscussionState =
    copy(
      data =
        data + (discussion.id -> discussion), // Only add if new topic title
    )

  def apply(
    discussionAction: DiscussionActionConfirmed,
  ): DiscussionState =
    copy(data = discussionAction match
      case Rejected(action) =>
        data // This should never actually get passed in, right?
      case DiscussionActionConfirmed.UpdateRoomSlot(topicId,
                                                    roomSlot,
          ) =>
        data.updatedWith(topicId) {
          _.map(value => value.copy(roomSlot = Some(roomSlot)))
        }
      case DiscussionActionConfirmed.Unschedule(topicId) =>
        data.updatedWith(topicId) {
          _.map(value => value.copy(roomSlot = None))
        }
      case DiscussionActionConfirmed.Delete(topicId) =>
        data.filterNot(_._2.id == topicId)
      case DiscussionActionConfirmed.Vote(topicId, voter) =>
        data.updatedWith(topicId) {
          _.map(value =>
            value.copy(interestedParties =
              value.interestedParties + voter,
            ),
          )
        }
      case DiscussionActionConfirmed.RemoveVote(topicId, voter) =>
        data.updatedWith(topicId) {
          _.map(value =>
            value.copy(interestedParties =
              value.interestedParties.filterNot(_.voter == voter),
            ),
          )
        }
      case DiscussionActionConfirmed.Rename(topicId, newTopic) =>
        data.updatedWith(topicId) {
          _.map(value => value.copy(topic = newTopic))
        }
      case DiscussionActionConfirmed.MoveTopic(topicId, targetRoomSlot) =>
        data.updatedWith(topicId) {
          _.map(value => value.copy(roomSlot = Some(targetRoomSlot)))
        }

      case DiscussionActionConfirmed.SwapTopics(topic1, topic2) =>
        val roomSlot1 = data.get(topic1).flatMap(_.roomSlot)
        val roomSlot2 = data.get(topic2).flatMap(_.roomSlot)
        data
          .updatedWith(topic1)(_.map(_.copy(roomSlot = roomSlot2)))
          .updatedWith(topic2)(_.map(_.copy(roomSlot = roomSlot1)))

      case DiscussionActionConfirmed.AddResult(discussion) =>
        data + (discussion.id -> discussion),
    )

object DiscussionState:
  def apply(
    slots: List[DaySlots],
    input: Discussion*,
  ): DiscussionState =
    val startingState = Map(
      input.map(d => (d.id, d))*,
    )

    DiscussionState(
      slots, // Instead of prepoulating slots here, it should be derived from the discussions, namely discussion.roomSlot
      startingState,
    )

  val timeSlotExamples =
    List(
      DaySlots(
        LocalDate.of(2025, 6, 24),
        List(
          TimeSlotForAllRooms(
            TimeSlot("8:00-8:50",
                     LocalDateTime.parse("2025-06-24T08:00:00"),
                     LocalDateTime.parse("2025-06-24T08:50:00"),
            ),
            List(Room.king,
                 Room.hawk,
                 Room.artGallery,
                 Room.danceHall,
            ),
          ),
          TimeSlotForAllRooms(
            TimeSlot("9:20-10:10",
                     LocalDateTime.parse("2025-06-24T09:20:00"),
                     LocalDateTime.parse("2025-06-24T10:10:00"),
            ),
            List(Room.king,
                 Room.hawk,
                 Room.artGallery,
                 Room.danceHall,
            ),
          ),
          TimeSlotForAllRooms(
            TimeSlot("10:30-11:20",
                     LocalDateTime.parse("2025-06-24T10:30:00"),
                     LocalDateTime.parse("2025-06-24T11:20:00"),
            ),
            List(Room.king,
                 Room.hawk,
                 Room.artGallery,
                 Room.danceHall,
            ),
          ),
        ),
      ),
      DaySlots(
        LocalDate.of(2025, 6, 25),
        List(
          TimeSlotForAllRooms(
            TimeSlot("8:00-8:50",
                     LocalDateTime.parse("2025-06-25T08:00:00"),
                     LocalDateTime.parse("2025-06-25T08:50:00"),
            ),
            List(Room.king,
                 Room.hawk,
                 Room.artGallery,
                 Room.danceHall,
            ),
          ),
          TimeSlotForAllRooms(
            TimeSlot("9:20-10:10",
                     LocalDateTime.parse("2025-06-25T09:20:00"),
                     LocalDateTime.parse("2025-06-25T10:10:00"),
            ),
            List(Room.king,
                 Room.hawk,
                 Room.artGallery,
                 Room.danceHall,
            ),
          ),
          TimeSlotForAllRooms(
            TimeSlot("10:30-11:20",
                     LocalDateTime.parse("2025-06-25T10:30:00"),
                     LocalDateTime.parse("2025-06-25T11:20:00"),
            ),
            List(Room.king,
                 Room.hawk,
                 Room.artGallery,
                 Room.danceHall,
            ),
          ),
        ),
      ),
      DaySlots(
        LocalDate.of(2025, 6, 27),
        List(
          TimeSlotForAllRooms(
            TimeSlot("8:00-8:50",
                     LocalDateTime.parse("2025-06-27T08:00:00"),
                     LocalDateTime.parse("2025-06-27T08:50:00"),
            ),
            List(Room.king,
                 Room.hawk,
                 Room.artGallery,
                 Room.danceHall,
            ),
          ),
          TimeSlotForAllRooms(
            TimeSlot("9:20-10:10",
                     LocalDateTime.parse("2025-06-27T09:20:00"),
                     LocalDateTime.parse("2025-06-27T10:10:00"),
            ),
            List(Room.king,
                 Room.hawk,
                 Room.artGallery,
                 Room.danceHall,
            ),
          ),
          TimeSlotForAllRooms(
            TimeSlot("10:30-11:20",
                     LocalDateTime.parse("2025-06-27T10:30:00"),
                     LocalDateTime.parse("2025-06-27T11:20:00"),
            ),
            List(Room.king,
                 Room.hawk,
                 Room.artGallery,
                 Room.danceHall,
            ),
          ),
        ),
      ),
    )

  val exampleWithDiscussions =
    DiscussionState(
      timeSlotExamples,
      Discussion.example1,
      Discussion.example2,
      Discussion.example3,
      Discussion.example4,
      Discussion.example5,
      Discussion.example6,
    )

  val example =
    DiscussionState(
      timeSlotExamples,
    )
