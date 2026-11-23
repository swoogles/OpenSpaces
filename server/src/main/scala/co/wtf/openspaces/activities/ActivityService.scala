package co.wtf.openspaces.activities

import co.wtf.openspaces.{Member, Person, UserOps}
import co.wtf.openspaces.db.{
  ActivityDismissalRepository,
  ActivityInterestRepository,
  ActivityRepository,
  ActivityRow,
  TimeSlotRepository,
  UserRepository,
}
import co.wtf.openspaces.github.GitHubProfileService
import neotype.unwrap
import zio.*
import java.time.{Instant, LocalDateTime, OffsetDateTime, ZoneOffset}

case class ActivityService(
  state: Ref[ActivityState],
  activityRepo: ActivityRepository,
  activityInterestRepo: ActivityInterestRepository,
  activityDismissalRepo: ActivityDismissalRepository,
  timeSlotRepo: TimeSlotRepository,
  userRepo: UserRepository,
  gitHubProfileService: GitHubProfileService,
  lock: Semaphore,
):
  private val AllowedDayBuffer = 3L

  def snapshot: UIO[ActivityState] =
    state.get

  def reloadFromDatabase: Task[ActivityState] =
    lock.withPermit:
      for
        reloaded <- ActivityService.loadInitialState(activityRepo, activityInterestRepo, activityDismissalRepo, gitHubProfileService)
        _ <- state.set(reloaded)
      yield reloaded

  def applyConfirmed(action: ActivityActionConfirmed): UIO[Unit] =
    state.update(_(action))

  /** Actions are applied one at a time, so validation always sees the latest state. */
  def applyAction(action: ActivityAction): Task[ActivityActionConfirmed] =
    lock.withPermit(applyActionLocked(action))

  private def applyActionLocked(action: ActivityAction): Task[ActivityActionConfirmed] =
    action match
      case create @ ActivityAction.Create(description, eventTime, creator) =>
        for
          _ <- UserOps.ensureUserExists(creator.unwrap, userRepo, gitHubProfileService)
          inRange <- isEventTimeInAllowedWindow(eventTime)
          result <- if !inRange then
            ZIO.succeed(ActivityActionConfirmed.Rejected(create))
          else
            for
              activity <- createActivity(description, eventTime, creator)
              _ <- activityRepo.insert(toRow(activity))
              _ <- activityInterestRepo.addInterest(activity.id.unwrap, creator.unwrap)
              _ <- state.update(_(ActivityActionConfirmed.Created(activity)))
            yield ActivityActionConfirmed.Created(activity)
        yield result

      case setInterest @ ActivityAction.SetInterest(activityId, person, interested) =>
        for
          _ <- UserOps.ensureUserExists(person.unwrap, userRepo, gitHubProfileService)
          current <- state.get
          result <- current.activities.get(activityId) match
            case None =>
              ZIO.succeed(ActivityActionConfirmed.Rejected(setInterest))
            case Some(activity) =>
              if interested then
                // Interested: add to members (remove from dismissed if applicable)
                if activity.hasMember(person) then
                  ZIO.succeed(ActivityActionConfirmed.Rejected(setInterest))
                else
                  for
                    _ <- activityDismissalRepo.removeDismissal(activityId.unwrap, person.unwrap)
                    row <- activityInterestRepo.addInterest(activityId.unwrap, person.unwrap)
                    joinedAtEpochMs = Some(row.interestedAt.toInstant.toEpochMilli)
                    confirmed = ActivityActionConfirmed.InterestSet(
                      activityId,
                      person,
                      interested = true,
                      joinedAtEpochMs = joinedAtEpochMs,
                      newOwner = None,
                    )
                    _ <- state.update(_(confirmed))
                  yield confirmed
              else
                // Not interested: either leave (if member) or dismiss (if never voted)
                if activity.hasDismissed(person) then
                  // Already dismissed, reject
                  ZIO.succeed(ActivityActionConfirmed.Rejected(setInterest))
                else if activity.hasMember(person) then
                  // Was a member, leaving
                  val shouldDelete = activity.wouldBeDeletedIfLeaves(person)
                  val nextOwner = if activity.isOwner(person) then activity.nextOwner else None
                  for
                    _ <- activityInterestRepo.removeInterest(activityId.unwrap, person.unwrap)
                    _ <- activityDismissalRepo.addDismissal(activityId.unwrap, person.unwrap)
                    confirmed <- if shouldDelete then
                      for
                        row <- activityRepo.findById(activityId.unwrap)
                        _ <- activityRepo.softDelete(activityId.unwrap)
                      yield ActivityActionConfirmed.Deleted(
                        activityId,
                        row.flatMap(_.slackChannelId),
                        row.flatMap(_.slackThreadTs),
                      )
                    else
                      for
                        _ <- ZIO.foreachDiscard(nextOwner)(owner =>
                          activityRepo.updateOwner(activityId.unwrap, owner.unwrap),
                        )
                      yield ActivityActionConfirmed.InterestSet(
                        activityId,
                        person,
                        interested = false,
                        joinedAtEpochMs = None,
                        newOwner = nextOwner,
                      )
                    _ <- state.update(_(confirmed))
                  yield confirmed
                else
                  // Never voted, just dismissing
                  for
                    _ <- activityDismissalRepo.addDismissal(activityId.unwrap, person.unwrap)
                    confirmed = ActivityActionConfirmed.InterestSet(
                      activityId,
                      person,
                      interested = false,
                      joinedAtEpochMs = None,
                      newOwner = None,
                    )
                    _ <- state.update(_(confirmed))
                  yield confirmed
        yield result

      case update @ ActivityAction.Update(activityId, newDescription, newEventTime, editor) =>
        for
          current <- state.get
          result <- current.activities.get(activityId) match
            case None =>
              ZIO.succeed(ActivityActionConfirmed.Rejected(update))
            case Some(existing) if existing.creator != editor =>
              ZIO.succeed(ActivityActionConfirmed.Unauthorized(update))
            case Some(existing) =>
              for
                inRange <- isEventTimeInAllowedWindow(newEventTime)
                confirmed <- if !inRange then
                  ZIO.succeed(ActivityActionConfirmed.Rejected(update))
                else
                  for
                    _ <- activityRepo.update(
                      toRow(existing.copy(description = newDescription, eventTime = newEventTime)).copy(
                        updatedAt = OffsetDateTime.now(),
                      ),
                    )
                    confirmed = ActivityActionConfirmed.Updated(
                      activityId,
                      newDescription,
                      newEventTime,
                    )
                    _ <- state.update(_(confirmed))
                  yield confirmed
              yield confirmed
        yield result

      case delete @ ActivityAction.Delete(activityId, requester) =>
        for
          current <- state.get
          result <- current.activities.get(activityId) match
            case None =>
              ZIO.succeed(ActivityActionConfirmed.Rejected(delete))
            case Some(existing) if existing.creator != requester =>
              ZIO.succeed(ActivityActionConfirmed.Unauthorized(delete))
            case Some(_) =>
              for
                row <- activityRepo.findById(activityId.unwrap)
                _ <- activityRepo.softDelete(activityId.unwrap)
                confirmed = ActivityActionConfirmed.Deleted(
                  activityId,
                  row.flatMap(_.slackChannelId),
                  row.flatMap(_.slackThreadTs),
                )
                _ <- state.update(_(confirmed))
              yield confirmed
        yield result

  /** Event times must fall within the conference time slots, padded by a few days. */
  def allowedEventWindow: Task[Option[(LocalDateTime, LocalDateTime)]] =
    timeSlotRepo.findStartBounds.map(_.map { case (minStart, maxStart) =>
      (minStart.minusDays(AllowedDayBuffer), maxStart.plusDays(AllowedDayBuffer))
    })

  private def isEventTimeInAllowedWindow(eventTime: LocalDateTime): Task[Boolean] =
    allowedEventWindow.map(_.exists { case (allowedStart, allowedEnd) =>
      !eventTime.isBefore(allowedStart) && !eventTime.isAfter(allowedEnd)
    })

  private def createActivity(
    description: ActivityDescription,
    eventTime: LocalDateTime,
    creator: Person,
  ): Task[Activity] =
    for
      id <- UserOps.generateId
      creatorUser <- userRepo.findByUsername(creator.unwrap)
      createdAtEpochMs <- UserOps.nowMillis
    yield Activity(
      id = ActivityId(id),
      description = description,
      creator = creator,
      creatorDisplayName = creatorUser.flatMap(_.displayName),
      eventTime = eventTime,
      members = List(Member(creator, createdAtEpochMs)),
      createdAtEpochMs = createdAtEpochMs,
    )

  private def toRow(activity: Activity): ActivityRow =
    val createdAt = OffsetDateTime.ofInstant(
      Instant.ofEpochMilli(activity.createdAtEpochMs),
      ZoneOffset.UTC,
    )
    ActivityRow(
      id = activity.id.unwrap,
      description = activity.description.unwrap,
      creator = activity.creator.unwrap,
      eventTime = activity.eventTime,
      createdAt = createdAt,
      updatedAt = OffsetDateTime.now(),
      deletedAt = None,
      slackChannelId = None,
      slackThreadTs = None,
      slackPermalink = activity.slackThreadUrl,
    )

object ActivityService:
  private def fromRow(
    row: ActivityRow,
    displayName: Option[String],
    members: List[Member],
    dismissedBy: Set[Person],
  ): Activity =
    Activity(
      id = ActivityId(row.id),
      description = ActivityDescription.make(row.description).getOrElse(ActivityDescription.unsafeMake(row.description)),
      creator = Person(row.creator),
      creatorDisplayName = displayName,
      eventTime = row.eventTime,
      members = members,
      createdAtEpochMs = row.createdAt.toInstant.toEpochMilli,
      slackThreadUrl = row.slackPermalink,
      dismissedBy = dismissedBy,
    )

  def loadInitialState(
    activityRepo: ActivityRepository,
    activityInterestRepo: ActivityInterestRepository,
    activityDismissalRepo: ActivityDismissalRepository,
    gitHubProfileService: GitHubProfileService,
  ): Task[ActivityState] =
    for
      rows <- activityRepo.findAllActive
      activities <- ZIO.foreach(rows) { row =>
        for
          creator <- gitHubProfileService.ensureUserWithDisplayName(row.creator).either.map(_.toOption)
          interests <- activityInterestRepo.findByActivity(row.id)
          dismissals <- activityDismissalRepo.findByActivity(row.id)
          members = interests
            .map(i =>
              Member(
                person = Person(i.githubUsername),
                joinedAtEpochMs = i.interestedAt.toInstant.toEpochMilli,
              ),
            )
            .toList
            .sortBy(_.joinedAtEpochMs)
          dismissedBy = dismissals.map(d => Person(d.githubUsername)).toSet
          creatorPerson = Person(row.creator)
          normalizedMembers =
            if members.exists(_.person == creatorPerson) then members
            else Member(creatorPerson, row.createdAt.toInstant.toEpochMilli) :: members
        yield fromRow(row, creator.flatMap(_.displayName), normalizedMembers, dismissedBy)
      }
    yield ActivityState(activities.map(a => a.id -> a).toMap)

  val layer: ZLayer[
    ActivityRepository & ActivityInterestRepository & ActivityDismissalRepository & TimeSlotRepository & UserRepository & GitHubProfileService,
    Throwable,
    ActivityService,
  ] =
    ZLayer.fromZIO:
      for
        activityRepo <- ZIO.service[ActivityRepository]
        activityInterestRepo <- ZIO.service[ActivityInterestRepository]
        activityDismissalRepo <- ZIO.service[ActivityDismissalRepository]
        timeSlotRepo <- ZIO.service[TimeSlotRepository]
        userRepo <- ZIO.service[UserRepository]
        gitHubProfileService <- ZIO.service[GitHubProfileService]
        initialState <- loadInitialState(activityRepo, activityInterestRepo, activityDismissalRepo, gitHubProfileService)
        stateRef <- Ref.make(initialState)
        lock <- Semaphore.make(1)
      yield ActivityService(
        stateRef,
        activityRepo,
        activityInterestRepo,
        activityDismissalRepo,
        timeSlotRepo,
        userRepo,
        gitHubProfileService,
        lock,
      )
