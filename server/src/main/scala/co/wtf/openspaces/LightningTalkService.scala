package co.wtf.openspaces

import co.wtf.openspaces.db.{LightningTalkRepository, LightningTalkRow, UserRepository}
import neotype.unwrap
import zio.*
import zio.json.*
import scala.util.Random as ScalaRandom

case class LightningTalkService(
  state: Ref[LightningTalkState],
  lightningTalkRepo: LightningTalkRepository,
  userRepo: UserRepository,
):
  // Pool of real GitHub users for random action generation.
  // TODO Dedup with other user pools
  private val randomUserPool: List[Person] = List(
    Person("kitlangton"),
    Person("jamesward"),
    Person("BruceEckel"),
    Person("cheshire137"),
    Person("gaearon"),
    Person("frenck"),
    Person("charliermarsh"),
    Person("peppy"),
    Person("phodal"),
    Person("dtolnay"),
    Person("GrahamCampbell"),
    Person("freekmurze"),
    Person("Borda"),
    Person("antfu"),
    Person("lllyasviel"),
    Person("fabpot"),
    Person("himself65"),
    Person("bradfitz"),
    Person("ornicar"),
  )

  def snapshot: UIO[LightningTalkState] =
    state.get

  def applyConfirmed(action: LightningTalkActionConfirmed): UIO[Unit] =
    state.update(_(action))

  def listProposals: UIO[List[LightningTalkProposal]] =
    snapshot.map(
      _.proposals.values.toList.sortBy(p => (p.createdAtEpochMs, p.id.unwrap)),
    )

  def applyAction(
    action: LightningTalkAction,
  ): Task[LightningTalkActionConfirmed] =
    action match
      case submit @ LightningTalkAction.Submit(topic, speaker) =>
        for
          _ <- ensureUserExists(speaker.unwrap)
          current <- state.get
          result <- current.proposalForSpeaker(speaker) match
            case Some(_) =>
              ZIO.succeed(LightningTalkActionConfirmed.Rejected(submit))
            case None =>
              for
                proposal <- createProposal(topic, speaker)
                _ <- persistProposal(proposal)
                _ <- state.update(_(proposal))
              yield LightningTalkActionConfirmed.AddResult(proposal)
        yield result

      case rename @ LightningTalkAction.Rename(proposalId, newTopic) =>
        for
          current <- state.get
          result <- current.proposals.get(proposalId) match
            case None =>
              ZIO.succeed(LightningTalkActionConfirmed.Rejected(rename))
            case Some(existing) =>
              for
                _ <- updateProposal(existing.copy(topic = newTopic))
                confirmed = LightningTalkActionConfirmed.Rename(proposalId, newTopic)
                _ <- state.update(_(confirmed))
              yield confirmed
        yield result

      case delete @ LightningTalkAction.Delete(proposalId) =>
        for
          current <- state.get
          result <- current.proposals.get(proposalId) match
            case None =>
              ZIO.succeed(LightningTalkActionConfirmed.Rejected(delete))
            case Some(_) =>
              for
                _ <- lightningTalkRepo.softDelete(proposalId.unwrap)
                confirmed = LightningTalkActionConfirmed.Delete(proposalId)
                _ <- state.update(_(confirmed))
              yield confirmed
        yield result

      case setAssignment @ LightningTalkAction.SetAssignment(
            proposalId,
            expectedCurrentAssignment,
            newAssignment,
          ) =>
        for
          current <- state.get
          result <- current.proposals.get(proposalId) match
            case None =>
              ZIO.succeed(LightningTalkActionConfirmed.Rejected(setAssignment))
            case Some(existing) =>
              val currentMatches = existing.assignment == expectedCurrentAssignment
              val targetOccupied = newAssignment.exists(
                assignment =>
                  current.assignmentOccupied(
                    assignment,
                    excludingProposalId = Some(proposalId),
                  ),
              )
              if !currentMatches || targetOccupied then
                ZIO.succeed(LightningTalkActionConfirmed.Rejected(setAssignment))
              else
                for
                  _ <- updateProposal(existing.copy(assignment = newAssignment))
                  confirmed = LightningTalkActionConfirmed.SetAssignment(
                    proposalId,
                    newAssignment,
                  )
                  _ <- state.update(_(confirmed))
                yield confirmed
        yield result

      case draw @ LightningTalkAction.DrawForNextNight =>
        for
          current <- state.get
          shuffled = ScalaRandom.shuffle(current.unassignedProposals)
          result <- LightningTalkDraw.drawForNextNight(current, _ => shuffled) match
            case Left(_) =>
              ZIO.succeed(LightningTalkActionConfirmed.Rejected(draw))
            case Right(drawResult) =>
              val confirmed = LightningTalkActionConfirmed.DrawForNightResult(
                drawResult.night,
                drawResult.assignments,
              )
              for
                _ <- ZIO.foreachDiscard(drawResult.assignments) { assignment =>
                  current.proposals.get(assignment.proposalId) match
                    case Some(proposal) =>
                      updateProposal(proposal.copy(assignment = Some(assignment.assignment)))
                    case None =>
                      ZIO.unit
                }
                _ <- state.update(_(confirmed))
              yield confirmed
        yield result

  def randomLightningAction: Task[LightningTalkActionConfirmed] =
    for
      person <- randomPerson
      current <- state.get
      topic <- DiscussionTopics.randomTopic
      action = current.proposalForSpeaker(person) match
        case Some(existing) =>
          LightningTalkAction.Rename(existing.id, topic)
        case None =>
          LightningTalkAction.Submit(topic, person)
      result <- applyAction(action)
    yield result

  private def ensureUserExists(username: String): Task[Unit] =
    userRepo.upsert(username, None).unit

  private def randomPerson: Task[Person] =
    for
      idx <- zio.Random.nextIntBounded(randomUserPool.size)
    yield randomUserPool(idx)

  private def createProposal(topic: Topic, speaker: Person): Task[LightningTalkProposal] =
    for
      id <- zio.Random.nextLong.map { n =>
        if n == Long.MinValue then 0L else math.abs(n)
      }
      speakerUser <- userRepo.findByUsername(speaker.unwrap)
      createdAtEpochMs = java.lang.System.currentTimeMillis()
    yield LightningTalkProposal(
      id = LightningTalkId(id),
      topic = topic,
      speaker = speaker,
      speakerDisplayName = speakerUser.flatMap(_.displayName),
      assignment = None,
      createdAtEpochMs = createdAtEpochMs,
    )

  private def persistProposal(proposal: LightningTalkProposal): Task[Unit] =
    lightningTalkRepo.insert(toRow(proposal))

  private def updateProposal(proposal: LightningTalkProposal): Task[Unit] =
    lightningTalkRepo.update(toRow(proposal))

  private def toRow(proposal: LightningTalkProposal): LightningTalkRow =
    val (night, slot) =
      proposal.assignment match
        case Some(assignment) =>
          (Some(assignment.night.toString), Some(assignment.slot.unwrap))
        case None =>
          (None, None)
    val createdAt = java.time.OffsetDateTime.ofInstant(
      java.time.Instant.ofEpochMilli(proposal.createdAtEpochMs),
      java.time.ZoneOffset.UTC,
    )
    LightningTalkRow(
      id = proposal.id.unwrap,
      topic = proposal.topic.unwrap,
      speaker = proposal.speaker.unwrap,
      assignmentNight = night,
      assignmentSlot = slot,
      createdAt = createdAt,
      updatedAt = java.time.OffsetDateTime.now(),
      deletedAt = None,
    )

object LightningTalkService:
  private def fromRow(
    row: LightningTalkRow,
    speakerDisplayName: Option[String],
  ): Option[LightningTalkProposal] =
    for
      topic <- Topic.make(row.topic).toOption
      assignment <- row.assignmentNight match
        case None =>
          Some(None)
        case Some(nightRaw) =>
          val parsedNight = LightningTalkNight.values.find(_.toString == nightRaw)
          val parsedSlot = row.assignmentSlot.flatMap(LightningTalkSlot.make(_).toOption)
          parsedNight.zip(parsedSlot).map { case (night, slot) =>
            Some(LightningAssignment(night, slot))
          }
    yield LightningTalkProposal(
      id = LightningTalkId(row.id),
      topic = topic,
      speaker = Person(row.speaker),
      speakerDisplayName = speakerDisplayName,
      assignment = assignment,
      createdAtEpochMs = row.createdAt.toInstant.toEpochMilli,
    )

  def loadInitialState(
    lightningTalkRepo: LightningTalkRepository,
    userRepo: UserRepository,
  ): Task[LightningTalkState] =
    for
      rows <- lightningTalkRepo.findAllActive
      speakers = rows.map(_.speaker).distinct
      userRows <- ZIO.foreach(speakers)(userRepo.findByUsername)
      userMap = userRows.flatten.map(u => u.githubUsername -> u.displayName).toMap
      proposals = rows.flatMap(row => fromRow(row, userMap.get(row.speaker).flatten))
    yield LightningTalkState(proposals.map(p => p.id -> p).toMap)

  val layer: ZLayer[LightningTalkRepository & UserRepository, Throwable, LightningTalkService] =
    ZLayer.fromZIO:
      for
        lightningTalkRepo <- ZIO.service[LightningTalkRepository]
        userRepo <- ZIO.service[UserRepository]
        initialState <- loadInitialState(lightningTalkRepo, userRepo)
        stateRef <- Ref.make(initialState)
      yield LightningTalkService(
        stateRef,
        lightningTalkRepo,
        userRepo,
      )
