package co.wtf.openspaces

import co.wtf.openspaces.db.*
import co.wtf.openspaces.discussions.{DiscussionState, DiscussionStore, VotePosition}
import neotype.unwrap
import zio.*

import java.time.OffsetDateTime

/** In-memory implementations of the repositories behind PersistentDiscussionStore,
  * so tests exercise the production discussion logic without a database.
  */
final case class InMemoryDiscussionDb(
  rooms: Vector[RoomRow],
  timeSlots: Vector[TimeSlotRow],
  discussions: Ref[Map[Long, DiscussionRow]],
  votes: Ref[Map[(Long, String), TopicVoteRow]],
  events: Ref[Vector[DiscussionEventRow]],
):
  val roomRepository: RoomRepository = new RoomRepository:
    def findAll: Task[Vector[RoomRow]] = ZIO.succeed(rooms)
    def findById(id: Int): Task[Option[RoomRow]] = ZIO.succeed(rooms.find(_.id == id))

  val timeSlotRepository: TimeSlotRepository = new TimeSlotRepository:
    def findAll: Task[Vector[TimeSlotRow]] = ZIO.succeed(timeSlots)
    def findById(id: Int): Task[Option[TimeSlotRow]] = ZIO.succeed(timeSlots.find(_.id == id))
    def findByRoomId(roomId: Int): Task[Vector[TimeSlotRow]] = ZIO.succeed(timeSlots.filter(_.roomId == roomId))
    def findStartBounds: Task[Option[(java.time.LocalDateTime, java.time.LocalDateTime)]] =
      ZIO.succeed(
        Option.when(timeSlots.nonEmpty)(
          (timeSlots.map(_.startTime).min, timeSlots.map(_.startTime).max),
        ),
      )

  val discussionRepository: DiscussionRepository = new DiscussionRepository:
    def findById(id: Long): Task[Option[DiscussionRow]] = discussions.get.map(_.get(id))
    def findAllActive: Task[Vector[DiscussionRow]] =
      discussions.get.map(_.values.filter(_.deletedAt.isEmpty).toVector)
    def insert(row: DiscussionRow): Task[Unit] = discussions.update(_ + (row.id -> row))
    def update(row: DiscussionRow): Task[Unit] = discussions.update(_ + (row.id -> row))
    def softDelete(id: Long): Task[Boolean] =
      discussions.modify { current =>
        current.get(id).filter(_.deletedAt.isEmpty) match
          case Some(row) => (true, current + (id -> row.copy(deletedAt = Some(OffsetDateTime.now()))))
          case None      => (false, current)
      }
    def truncate: Task[Unit] = discussions.set(Map.empty)
    def updateSlackThread(topicId: Long, channelId: String, threadTs: String, permalink: String): Task[Unit] =
      discussions.update(_.updatedWith(topicId)(_.map(
        _.copy(slackChannelId = Some(channelId), slackThreadTs = Some(threadTs), slackPermalink = Some(permalink)),
      )))
    def updateRoomSlot(topicId: Long, roomId: Option[Int], timeSlotId: Option[Int]): Task[Unit] =
      discussions.update(_.updatedWith(topicId)(_.map(_.copy(roomId = roomId, timeSlotId = timeSlotId))))

  val topicVoteRepository: TopicVoteRepository = new TopicVoteRepository:
    def findAllForActiveDiscussions: Task[Vector[TopicVoteRow]] =
      for
        active <- discussions.get.map(_.values.filter(_.deletedAt.isEmpty).map(_.id).toSet)
        all    <- votes.get
      yield all.values.filter(v => active.contains(v.topicId)).toVector
    def upsertVote(topicId: Long, githubUsername: String, position: VotePosition): Task[TopicVoteRow] =
      votes.modify { current =>
        val firstVotedAt = current.get((topicId, githubUsername)).map(_.firstVotedAt).getOrElse(OffsetDateTime.now())
        val row = TopicVoteRow(topicId, githubUsername, position.toString, firstVotedAt)
        (row, current + ((topicId, githubUsername) -> row))
      }
    def deleteVote(topicId: Long, githubUsername: String): Task[Unit] =
      votes.update(_ - ((topicId, githubUsername)))

  val eventRepository: EventRepository = new EventRepository:
    def append(eventType: String, topicId: Long, payload: String, actor: String): Task[DiscussionEventRow] =
      events.modify { current =>
        val row = DiscussionEventRow.create(eventType, topicId, payload, actor).copy(id = current.size.toLong + 1)
        (row, current :+ row)
      }
    def findByTopicId(topicId: Long): Task[Vector[DiscussionEventRow]] = events.get.map(_.filter(_.topicId == topicId))
    def findByActor(actor: String): Task[Vector[DiscussionEventRow]] = events.get.map(_.filter(_.actor == actor))
    def findAll: Task[Vector[DiscussionEventRow]] = events.get
    def findAllOrdered: Task[Vector[DiscussionEventRow]] = events.get

object InMemoryDiscussionDb:

  /** Seed rooms, time slots, discussions and votes so that loading them back
    * through PersistentDiscussionStore reproduces `initial`.
    */
  def make(initial: DiscussionState): UIO[InMemoryDiscussionDb] =
    val roomRows =
      initial.slots
        .flatMap(_.slots.flatMap(_.rooms))
        .distinctBy(_.id)
        .map(r => RoomRow(r.id, r.name, r.capacity))
        .toVector
    val slotKeys =
      for
        day       <- initial.slots
        slotGroup <- day.slots
        room      <- slotGroup.rooms
      yield (room.id, slotGroup.time.startTime, slotGroup.time.endTime)
    val timeSlotRows = slotKeys.zipWithIndex.map { case ((roomId, start, end), i) =>
      TimeSlotRow(i + 1, roomId, start, end)
    }.toVector
    def timeSlotIdFor(roomSlot: RoomSlot): Option[Int] =
      timeSlotRows.collectFirst {
        case row
            if row.roomId == roomSlot.room.id &&
              row.startTime == roomSlot.timeSlot.startTime &&
              row.endTime == roomSlot.timeSlot.endTime =>
          row.id
      }
    val discussionRows = initial.data.values.map { d =>
      d.id.unwrap -> DiscussionRow.create(
        id = d.id.unwrap,
        topic = d.topic.unwrap,
        facilitator = d.facilitator.unwrap,
        glyphicon = d.glyphicon.name,
        roomId = d.roomSlot.map(_.room.id),
        timeSlotId = d.roomSlot.flatMap(timeSlotIdFor),
        isLockedTimeslot = d.lockedTimeslot,
      )
    }.toMap
    val voteRows = initial.data.values.flatMap { d =>
      d.interestedParties.map { f =>
        (d.id.unwrap, f.voter.unwrap) ->
          TopicVoteRow(d.id.unwrap, f.voter.unwrap, f.position.toString, OffsetDateTime.now())
      }
    }.toMap
    for
      discussions <- Ref.make(discussionRows)
      votes       <- Ref.make(voteRows)
      events      <- Ref.make(Vector.empty[DiscussionEventRow])
    yield InMemoryDiscussionDb(roomRows, timeSlotRows, discussions, votes, events)

  /** A PersistentDiscussionStore backed by an in-memory database seeded with `initial`. */
  def storeLayer(initial: DiscussionState = DiscussionState.empty): ULayer[DiscussionStore] =
    ZLayer.make[DiscussionStore](
      ZLayer.fromZIO(make(initial)).flatMap { env =>
        val db = env.get[InMemoryDiscussionDb]
        ZLayer.succeed(db.eventRepository) ++
          ZLayer.succeed(db.discussionRepository) ++
          ZLayer.succeed(db.topicVoteRepository) ++
          ZLayer.succeed(db.roomRepository) ++
          ZLayer.succeed(db.timeSlotRepository)
      },
      TestLayers.userRepositoryLayer,
      TestLayers.gitHubProfileServiceLayer,
      GlyphiconService.layer,
      PersistentDiscussionStore.layer,
    ).orDie
