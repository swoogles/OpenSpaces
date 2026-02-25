package co.wtf.openspaces.discussions

import co.wtf.openspaces.{TimeSlot, Room, TopicId, RoomSlot}
import co.wtf.openspaces.discussions.DiscussionActionConfirmed.Rejected

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
      case DiscussionActionConfirmed.StateReplace(discussions, newSlots) =>
        copy(
          slots = newSlots,
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
          case DiscussionActionConfirmed.SetLockedTimeslot(topicId,
                                                           lockedTimeslot,
              ) =>
            data.updatedWith(topicId) {
              _.map(value => value.copy(lockedTimeslot = lockedTimeslot))
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

          case DiscussionActionConfirmed.FacilitatorChanged(topicId, newFacilitator, newDisplayName) =>
            data.updatedWith(topicId) {
              _.map { value =>
                // Old facilitator voted NotInterested, hence the ownership transfer
                val oldFacilitator = value.facilitator
                val updatedParties = value.interestedParties
                  .filterNot(_.voter == oldFacilitator) + Feedback(oldFacilitator, VotePosition.NotInterested)
                value.copy(
                  facilitator = newFacilitator,
                  facilitatorDisplayName = newDisplayName,
                  interestedParties = updatedParties,
                )
              }
            }

          case DiscussionActionConfirmed
                .SlackThreadLinked(topicId, slackThreadUrl) =>
            data.updatedWith(topicId) {
              _.map(value =>
                value.copy(slackThreadUrl = Some(slackThreadUrl)),
              )
            }
          case DiscussionActionConfirmed.Unauthorized(_) =>
            data
          case DiscussionActionConfirmed.StateReplace(_, _) =>
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
      slots,
      startingState,
    )

  /** Empty state - slots will be loaded from database on server */
  val empty: DiscussionState = DiscussionState(List.empty, Map.empty)

  // Test slots - only used for examples/tests
  // Production slots come from the database (rooms + time_slots tables)
  // NOTE: Must be defined before example/exampleWithDiscussions to avoid initialization order issues
  private def exampleTimeSlot(start: String, end: String): TimeSlot =
    TimeSlot(LocalDateTime.parse(start), LocalDateTime.parse(end))

  private val exampleSlots: List[DaySlots] = List(
    DaySlots(
      LocalDate.of(2026, 3, 2), // Monday
      List(
        TimeSlotForAllRooms(
          exampleTimeSlot("2026-03-02T09:50:00", "2026-03-02T10:40:00"),
          Room.all,
        ),
        TimeSlotForAllRooms(
          exampleTimeSlot("2026-03-02T11:10:00", "2026-03-02T12:00:00"),
          Room.all,
        ),
      ),
    ),
    DaySlots(
      LocalDate.of(2026, 3, 3), // Tuesday
      List(
        TimeSlotForAllRooms(
          exampleTimeSlot("2026-03-03T09:00:00", "2026-03-03T09:50:00"),
          Room.all,
        ),
        TimeSlotForAllRooms(
          exampleTimeSlot("2026-03-03T10:20:00", "2026-03-03T11:10:00"),
          Room.all,
        ),
        TimeSlotForAllRooms(
          exampleTimeSlot("2026-03-03T11:40:00", "2026-03-03T12:30:00"),
          Room.all,
        ),
      ),
    ),
  )

  // Example for tests only - uses hardcoded slots
  // Real application loads slots from database
  val exampleWithDiscussions: DiscussionState =
    DiscussionState(
      exampleSlots,
      Discussion.example1,
      Discussion.example2,
      Discussion.example3,
      Discussion.example4,
      Discussion.example5,
      Discussion.example6,
    )

  val example: DiscussionState =
    DiscussionState(exampleSlots)
