package co.wtf.openspaces.activities

import co.wtf.openspaces.{Person, RandomUsers}
import co.wtf.openspaces.db.{
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
  timeSlotRepo: TimeSlotRepository,
  userRepo: UserRepository,
  gitHubProfileService: GitHubProfileService,
):
  private val AllowedDayBuffer = 3L

  def snapshot: UIO[ActivityState] =
    state.get

  def reloadFromDatabase: Task[ActivityState] =
    for
      reloaded <- ActivityService.loadInitialState(activityRepo, activityInterestRepo, gitHubProfileService)
      _ <- state.set(reloaded)
    yield reloaded

  def applyConfirmed(action: ActivityActionConfirmed): UIO[Unit] =
    state.update(_(action))

  def applyAction(action: ActivityAction): Task[ActivityActionConfirmed] =
    action match
      case create @ ActivityAction.Create(description, eventTime, creator) =>
        for
          _ <- ensureUserExists(creator.unwrap)
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
          _ <- ensureUserExists(person.unwrap)
          current <- state.get
          result <- current.activities.get(activityId) match
            case None =>
              ZIO.succeed(ActivityActionConfirmed.Rejected(setInterest))
            case Some(_) =>
              val persist =
                if interested then activityInterestRepo.addInterest(activityId.unwrap, person.unwrap).unit
                else activityInterestRepo.removeInterest(activityId.unwrap, person.unwrap)
              for
                _ <- persist
                confirmed = ActivityActionConfirmed.InterestSet(activityId, person, interested)
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

  def randomActivityAction: Task[ActivityActionConfirmed] =
    for
      person <- RandomUsers.randomPerson
      current <- state.get
      roll <- zio.Random.nextIntBounded(100)
      action <- current.activities.values.toList match
        case Nil =>
          makeRandomCreateAction(person)
        case activities =>
          if roll < 40 then makeRandomCreateAction(person)
          else if roll < 75 then
            val activity = activities(scala.util.Random.nextInt(activities.size))
            val interested = !activity.interestedPeople.contains(person)
            ZIO.succeed(ActivityAction.SetInterest(activity.id, person, interested))
          else if roll < 90 then
            current.ownedBy(person).headOption match
              case Some(owned) =>
                for
                  desc <- randomDescription
                  eventTime <- randomEventTimeInWindow
                yield ActivityAction.Update(owned.id, desc, eventTime, person)
              case None =>
                makeRandomCreateAction(person)
          else
            current.ownedBy(person).headOption match
              case Some(owned) => ZIO.succeed(ActivityAction.Delete(owned.id, person))
              case None => makeRandomCreateAction(person)
      result <- applyAction(action)
    yield result

  private def randomDescription: UIO[ActivityDescription] =
    val titles = List(
      "Pair on a side project",
      "Coffee walk and architecture chat",
      "Open source bug smash",
      "Mentoring hour",
      "Product teardown session",
      "Design critique huddle",
    )
    zio.Random.nextIntBounded(titles.size).map(i => ActivityDescription.unsafeMake(titles(i)))

  private def makeRandomCreateAction(person: Person): Task[ActivityAction.Create] =
    for
      desc <- randomDescription
      eventTime <- randomEventTimeInWindow
    yield ActivityAction.Create(desc, eventTime, person)

  private def randomEventTimeInWindow: Task[LocalDateTime] =
    timeSlotRepo.findStartBounds.flatMap {
      case Some((minStart, maxStart)) =>
        val start = minStart.minusDays(AllowedDayBuffer)
        val end = maxStart.plusDays(AllowedDayBuffer)
        val secondsRange = java.time.Duration.between(start, end).getSeconds.max(0L)
        zio.Random.nextLongBetween(0L, secondsRange + 1L).map(offset => start.plusSeconds(offset))
      case None =>
        ZIO.succeed(LocalDateTime.now(ZoneOffset.UTC))
    }

  private def isEventTimeInAllowedWindow(eventTime: LocalDateTime): Task[Boolean] =
    timeSlotRepo.findStartBounds.map {
      case Some((minStart, maxStart)) =>
        val allowedStart = minStart.minusDays(AllowedDayBuffer)
        val allowedEnd = maxStart.plusDays(AllowedDayBuffer)
        !eventTime.isBefore(allowedStart) && !eventTime.isAfter(allowedEnd)
      case None =>
        false
    }

  private def ensureUserExists(username: String): Task[Unit] =
    if username == "system" then userRepo.upsert(username, Some(username)).unit
    else gitHubProfileService.ensureUserWithDisplayName(username).unit

  private def createActivity(
    description: ActivityDescription,
    eventTime: LocalDateTime,
    creator: Person,
  ): Task[Activity] =
    for
      id <- zio.Random.nextLong.map(n => if n == Long.MinValue then 0L else math.abs(n))
      creatorUser <- userRepo.findByUsername(creator.unwrap)
      createdAtEpochMs = java.lang.System.currentTimeMillis()
    yield Activity(
      id = ActivityId(id),
      description = description,
      creator = creator,
      creatorDisplayName = creatorUser.flatMap(_.displayName),
      eventTime = eventTime,
      interestedPeople = Set(creator),
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
    interestedPeople: Set[Person],
  ): Activity =
    Activity(
      id = ActivityId(row.id),
      description = ActivityDescription.make(row.description).getOrElse(ActivityDescription.unsafeMake(row.description)),
      creator = Person(row.creator),
      creatorDisplayName = displayName,
      eventTime = row.eventTime,
      interestedPeople = interestedPeople,
      createdAtEpochMs = row.createdAt.toInstant.toEpochMilli,
      slackThreadUrl = row.slackPermalink,
    )

  def loadInitialState(
    activityRepo: ActivityRepository,
    activityInterestRepo: ActivityInterestRepository,
    gitHubProfileService: GitHubProfileService,
  ): Task[ActivityState] =
    for
      rows <- activityRepo.findAllActive
      activities <- ZIO.foreach(rows) { row =>
        for
          creator <- gitHubProfileService.ensureUserWithDisplayName(row.creator).either.map(_.toOption)
          interests <- activityInterestRepo.findByActivity(row.id)
          interestedPeople = interests.map(i => Person(i.githubUsername)).toSet + Person(row.creator)
        yield fromRow(row, creator.flatMap(_.displayName), interestedPeople)
      }
    yield ActivityState(activities.map(a => a.id -> a).toMap)

  val layer: ZLayer[
    ActivityRepository & ActivityInterestRepository & TimeSlotRepository & UserRepository & GitHubProfileService,
    Throwable,
    ActivityService,
  ] =
    ZLayer.fromZIO:
      for
        activityRepo <- ZIO.service[ActivityRepository]
        activityInterestRepo <- ZIO.service[ActivityInterestRepository]
        timeSlotRepo <- ZIO.service[TimeSlotRepository]
        userRepo <- ZIO.service[UserRepository]
        gitHubProfileService <- ZIO.service[GitHubProfileService]
        initialState <- loadInitialState(activityRepo, activityInterestRepo, gitHubProfileService)
        stateRef <- Ref.make(initialState)
      yield ActivityService(
        stateRef,
        activityRepo,
        activityInterestRepo,
        timeSlotRepo,
        userRepo,
        gitHubProfileService,
      )

