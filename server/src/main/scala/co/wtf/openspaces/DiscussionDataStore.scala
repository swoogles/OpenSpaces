package co.wtf.openspaces

import zio.*
import zio.direct.*

class DiscussionDataStore(
  discussionDatabase: Ref[DiscussionState],
  glyphiconService: GlyphiconService):
  def snapshot = discussionDatabase.get

  def applyAction(
    discussionAction: DiscussionAction,
  ): UIO[DiscussionActionConfirmed] =
    discussionAction match
      case DiscussionAction.Add(topic, facilitator) =>
        for {
          randomIcon <- glyphiconService.getRandomIcon
          randomId   <- Random.nextLong
          discussion = Discussion(
            topic,
            facilitator,
            Set(Feedback(facilitator, VotePosition.Interested)),
            TopicId(randomId),
            randomIcon,
            None,
          )
          res <- discussionDatabase.updateAndGet(s => s(discussion))
        } yield DiscussionActionConfirmed.AddResult(discussion)

      case swap @ DiscussionAction.SwapTopics(topic1,
                                              expectedSlot1,
                                              topic2,
                                              expectedSlot2,
          ) =>
        defer:
          val currentState = discussionDatabase.get.run
          val actualSlot1 =
            currentState.data.get(topic1).flatMap(_.roomSlot)
          val actualSlot2 =
            currentState.data.get(topic2).flatMap(_.roomSlot)

          // Validate that the room slots haven't changed since the user saw them
          val slotsMatch =
            actualSlot1.contains(expectedSlot1) &&
              actualSlot2.contains(expectedSlot2)

          if (slotsMatch)
            val confirmedAction =
              DiscussionActionConfirmed.fromDiscussionAction(swap)
            discussionDatabase
              .updateAndGet(s => s(confirmedAction))
              .run
            confirmedAction
          else DiscussionActionConfirmed.Rejected(swap)

      case move @ DiscussionAction.MoveTopic(topicId,
                                             targetRoomSlot,
          ) =>
        defer:
          val currentState = discussionDatabase.get.run
          // Check if target slot is occupied by a different discussion
          val targetOccupied = currentState.data.values.exists {
            discussion =>
              discussion.id != topicId &&
              discussion.roomSlot.contains(targetRoomSlot)
          }

          if (targetOccupied) DiscussionActionConfirmed.Rejected(move)
          else
            val confirmedAction =
              DiscussionActionConfirmed.fromDiscussionAction(move)
            discussionDatabase
              .updateAndGet(s => s(confirmedAction))
              .run
            confirmedAction
      // TODO Case for voting - IF a topic was deleted, the vote should be rejected.

      case other =>
        defer:
          val confirmedAction =
            DiscussionActionConfirmed.fromDiscussionAction(other)
          discussionDatabase.updateAndGet(s => s(confirmedAction)).run
          confirmedAction

  private def randomExistingTopicId =
    defer:
      val data = snapshot.run
      val idx =
        Random.nextIntBounded(data.data.keys.toList.length).run
      data.data.keys.toList(idx)

  def randomDiscussionAction =
    defer:
      val actionIdx = Random.nextIntBounded(10).run
      val noCurrentItems = snapshot.run.data.keys.toList.isEmpty
      val addNewDiscussion =
        val person =
          Person("RandomPerson - " + Random.nextIntBounded(20).run)
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
            case 2 | 3 | 4 =>
              val id = randomExistingTopicId.run
              val person = Person(
                "RandomPerson - " + Random.nextIntBounded(20).run,
              )
              DiscussionAction.Vote(
                id,
                Feedback(person, VotePosition.Interested),
              )

            case 5 | 6 | 7 =>
              val id = randomExistingTopicId.run
              // TODO Ensure existing person that has voted for topic, unless it's got 0 votes
              val person = Person(
                "RandomPerson - " + Random.nextIntBounded(20).run,
              )
              DiscussionAction.RemoveVote(id, person)
            case 8 =>
              val id = randomExistingTopicId.run
              val newTopic = DiscussionTopics.randomTopic.run
              DiscussionAction.Rename(id, newTopic)
            case 9 =>
              defer:
                val id = randomExistingTopicId.run
                val state = discussionDatabase.get.run
                val slots = state.slots
                // Find an available slot through some truly gory inline logic
                val availableSlot =
                  slots
                    .flatMap(slot =>
                      slot.slots.flatMap(timeSlotForAllRooms =>
                        timeSlotForAllRooms.rooms
                          .find(room =>
                            !state.data.values.exists(discussion =>
                              discussion.roomSlot.contains(
                                RoomSlot(room,
                                         timeSlotForAllRooms.time,
                                ),
                              ),
                            ),
                          )
                          .map(room =>
                            RoomSlot(
                              room,
                              timeSlotForAllRooms.time,
                            ),
                          ),
                      ),
                    )
                    .headOption
                availableSlot match {
                  case None =>
                    DiscussionAction.Delete(id)
                  case Some(roomSlot) =>
                    DiscussionAction.UpdateRoomSlot(
                      id,
                      roomSlot,
                    )
                }
              .run
          }
      applyAction(action).run

object DiscussionDataStore:
  def layer(
    useSampleData: Boolean,
  ) =
    ZLayer.fromZIO:
      defer:
        val initialState =
          if useSampleData then DiscussionState.exampleWithDiscussions
          else DiscussionState.example
        DiscussionDataStore(
          Ref.make(initialState).run,
          ZIO.service[GlyphiconService].run,
        )
        //
