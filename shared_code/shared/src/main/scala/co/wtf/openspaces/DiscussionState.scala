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
    discussionAction match
      case DiscussionActionConfirmed.StateReplace(discussions) =>
        copy(
          data = discussions.map(d => d.id -> d).toMap,
        )
      case other =>
        copy(data = other match
          case Rejected(action) =>
            data // This should never actually get passed in, right?
          case DiscussionActionConfirmed.SetRoomSlot(topicId,
                                                     newRoomSlot,
              ) =>
            data.updatedWith(topicId) {
              _.map(value => value.copy(roomSlot = newRoomSlot))
            }
          case DiscussionActionConfirmed.Delete(topicId) =>
            data.filterNot(_._2.id == topicId)
          case DiscussionActionConfirmed.Vote(topicId, newFeedback) =>
            // Upsert: remove any existing vote from this voter, then add the new vote
            data.updatedWith(topicId) {
              _.map(value =>
                value.copy(interestedParties =
                  value.interestedParties.filterNot(
                    _.voter == newFeedback.voter,
                  ) + newFeedback,
                ),
              )
            }
          case DiscussionActionConfirmed.ResetUser(person,
                                                   deletedTopicIds,
                                                   clearedVoteTopicIds,
              ) =>
            // Remove deleted topics and clear votes from affected topics
            val afterDeletes = data.filterNot { case (id, _) =>
              deletedTopicIds.contains(id)
            }
            clearedVoteTopicIds.foldLeft(afterDeletes) {
              (
                acc,
                topicId,
              ) =>
                acc.updatedWith(topicId) {
                  _.map(value =>
                    value.copy(interestedParties =
                      value.interestedParties.filterNot(
                        _.voter == person,
                      ),
                    ),
                  )
                }
            }
          case DiscussionActionConfirmed.Rename(topicId, newTopic) =>
            data.updatedWith(topicId) {
              _.map(value => value.copy(topic = newTopic))
            }
          case DiscussionActionConfirmed.SwapTopics(topic1,
                                                    newSlot1,
                                                    topic2,
                                                    newSlot2,
              ) =>
            // The confirmed action contains the new (swapped) room slots
            data
              .updatedWith(topic1)(
                _.map(_.copy(roomSlot = Some(newSlot1))),
              )
              .updatedWith(topic2)(
                _.map(_.copy(roomSlot = Some(newSlot2))),
              )

          case DiscussionActionConfirmed.AddResult(discussion) =>
            data + (discussion.id -> discussion)

          case DiscussionActionConfirmed
                .SlackThreadLinked(topicId, slackThreadUrl) =>
            data.updatedWith(topicId) {
              _.map(value =>
                value.copy(slackThreadUrl = Some(slackThreadUrl)),
              )
            }
          case DiscussionActionConfirmed.Unauthorized(_) =>
            data
          case DiscussionActionConfirmed.StateReplace(_) =>
            data,
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

  // Winter Tech Forum 2026: March 2-6, 2026
  // https://www.wintertechforum.com/schedule/
  // Monday: Intro + Session 2
  // Tuesday: Sessions 3-5
  // Wednesday: Hackathon (no Open Spaces)
  // Thursday: Sessions 6-8
  // Friday: Sessions 9-10 + Closing
  val timeSlotExamples =
    List(
      DaySlots(
        LocalDate.of(2026, 3, 2), // Monday
        List(
          TimeSlotForAllRooms(
            TimeSlot("9:50-10:40",
                     LocalDateTime.parse("2026-03-02T09:50:00"),
                     LocalDateTime.parse("2026-03-02T10:40:00"),
            ),
            List(Room.king,
                 Room.hawk,
                 Room.artGallery,
                 Room.danceHall,
            ),
          ),
          TimeSlotForAllRooms(
            TimeSlot("11:10-12:00",
                     LocalDateTime.parse("2026-03-02T11:10:00"),
                     LocalDateTime.parse("2026-03-02T12:00:00"),
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
        LocalDate.of(2026, 3, 3), // Tuesday
        List(
          TimeSlotForAllRooms(
            TimeSlot("9:00-9:50",
                     LocalDateTime.parse("2026-03-03T09:00:00"),
                     LocalDateTime.parse("2026-03-03T09:50:00"),
            ),
            List(Room.king,
                 Room.hawk,
                 Room.artGallery,
                 Room.danceHall,
            ),
          ),
          TimeSlotForAllRooms(
            TimeSlot("10:20-11:10",
                     LocalDateTime.parse("2026-03-03T10:20:00"),
                     LocalDateTime.parse("2026-03-03T11:10:00"),
            ),
            List(Room.king,
                 Room.hawk,
                 Room.artGallery,
                 Room.danceHall,
            ),
          ),
          TimeSlotForAllRooms(
            TimeSlot("11:40-12:30",
                     LocalDateTime.parse("2026-03-03T11:40:00"),
                     LocalDateTime.parse("2026-03-03T12:30:00"),
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
        LocalDate.of(2026, 3, 5), // Thursday (Wednesday is Hackathon)
        List(
          TimeSlotForAllRooms(
            TimeSlot("9:00-9:50",
                     LocalDateTime.parse("2026-03-05T09:00:00"),
                     LocalDateTime.parse("2026-03-05T09:50:00"),
            ),
            List(Room.king,
                 Room.hawk,
                 Room.artGallery,
                 Room.danceHall,
            ),
          ),
          TimeSlotForAllRooms(
            TimeSlot("10:20-11:10",
                     LocalDateTime.parse("2026-03-05T10:20:00"),
                     LocalDateTime.parse("2026-03-05T11:10:00"),
            ),
            List(Room.king,
                 Room.hawk,
                 Room.artGallery,
                 Room.danceHall,
            ),
          ),
          TimeSlotForAllRooms(
            TimeSlot("11:40-12:30",
                     LocalDateTime.parse("2026-03-05T11:40:00"),
                     LocalDateTime.parse("2026-03-05T12:30:00"),
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
        LocalDate.of(2026, 3, 6), // Friday
        List(
          TimeSlotForAllRooms(
            TimeSlot("9:00-9:50",
                     LocalDateTime.parse("2026-03-06T09:00:00"),
                     LocalDateTime.parse("2026-03-06T09:50:00"),
            ),
            List(Room.king,
                 Room.hawk,
                 Room.artGallery,
                 Room.danceHall,
            ),
          ),
          TimeSlotForAllRooms(
            TimeSlot("10:20-11:10",
                     LocalDateTime.parse("2026-03-06T10:20:00"),
                     LocalDateTime.parse("2026-03-06T11:10:00"),
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
