package co.wtf.openspaces

import co.wtf.openspaces.db.{LightningTalkRepository, LightningTalkRow, UserRepository}
import neotype.unwrap
import zio.*
import scala.util.Random as ScalaRandom

case class LightningTalkService(
  state: Ref[LightningTalkState],
  lightningTalkRepo: LightningTalkRepository,
  userRepo: UserRepository,
  gitHubProfileService: GitHubProfileService,
):
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
      case setParticipation @ LightningTalkAction.SetParticipation(
            speaker,
            participating,
          ) =>
        for
          _ <- ensureUserExists(speaker.unwrap)
          current <- state.get
          result <- (current.proposalForSpeaker(speaker), participating) match
            case (Some(_), true) =>
              ZIO.succeed(LightningTalkActionConfirmed.Rejected(setParticipation))
            case (None, false) =>
              ZIO.succeed(LightningTalkActionConfirmed.Rejected(setParticipation))
            case (None, true) =>
              for
                proposal <- createProposal(speaker)
                _ <- persistProposal(proposal)
                _ <- state.update(_(proposal))
              yield LightningTalkActionConfirmed.AddResult(proposal)
            case (Some(existing), false) =>
              for
                row <- lightningTalkRepo.findById(existing.id.unwrap)
                _ <- lightningTalkRepo.delete(existing.id.unwrap)
                confirmed = LightningTalkActionConfirmed.Delete(
                  existing.id,
                  row.flatMap(_.slackChannelId),
                  row.flatMap(_.slackThreadTs),
                )
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
      action = current.proposalForSpeaker(person) match
        case Some(_) =>
          LightningTalkAction.SetParticipation(person, participating = false)
        case None =>
          LightningTalkAction.SetParticipation(person, participating = true)
      result <- applyAction(action)
    yield result

  private def ensureUserExists(username: String): Task[Unit] =
    if username == "system" then
      userRepo.upsert(username, Some(username)).unit
    else
      gitHubProfileService.ensureUserWithDisplayName(username).unit

  private def randomPerson: UIO[Person] =
    RandomUsers.randomPerson

  private def createProposal(speaker: Person): Task[LightningTalkProposal] =
    for
      id <- zio.Random.nextLong.map { n =>
        if n == Long.MinValue then 0L else math.abs(n)
      }
      speakerUser <- userRepo.findByUsername(speaker.unwrap)
      createdAtEpochMs = java.lang.System.currentTimeMillis()
    yield LightningTalkProposal(
      id = LightningTalkId(id),
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
      speaker = proposal.speaker.unwrap,
      assignmentNight = night,
      assignmentSlot = slot,
      createdAt = createdAt,
      slackChannelId = None,
      slackThreadTs = None,
      slackPermalink = proposal.slackThreadUrl,
    )

object LightningTalkService:
  private def fromRow(
    row: LightningTalkRow,
    speakerDisplayName: Option[String],
  ): Option[LightningTalkProposal] =
    for
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
      speaker = Person(row.speaker),
      speakerDisplayName = speakerDisplayName,
      assignment = assignment,
      createdAtEpochMs = row.createdAt.toInstant.toEpochMilli,
      slackThreadUrl = row.slackPermalink,
    )

  def loadInitialState(
    lightningTalkRepo: LightningTalkRepository,
    gitHubProfileService: GitHubProfileService,
  ): Task[LightningTalkState] =
    for
      rows <- lightningTalkRepo.findAll
      speakers = rows.map(_.speaker).distinct
      userRows <- ZIO.foreach(speakers)(speaker =>
        gitHubProfileService.ensureUserWithDisplayName(speaker).either.map(_.toOption),
      )
      userMap = userRows.flatten.map(u => u.githubUsername -> u.displayName.orElse(Some(u.githubUsername))).toMap
      proposals = rows.flatMap(row => fromRow(row, userMap.get(row.speaker).flatten))
    yield LightningTalkState(proposals.map(p => p.id -> p).toMap)

  val layer: ZLayer[LightningTalkRepository & UserRepository & GitHubProfileService, Throwable, LightningTalkService] =
    ZLayer.fromZIO:
      for
        lightningTalkRepo <- ZIO.service[LightningTalkRepository]
        userRepo <- ZIO.service[UserRepository]
        gitHubProfileService <- ZIO.service[GitHubProfileService]
        initialState <- loadInitialState(lightningTalkRepo, gitHubProfileService)
        stateRef <- Ref.make(initialState)
      yield LightningTalkService(
        stateRef,
        lightningTalkRepo,
        userRepo,
        gitHubProfileService,
      )
