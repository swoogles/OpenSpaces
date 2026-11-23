package co.wtf.openspaces.chaos

import co.wtf.openspaces.{Person, RandomUsers}
import co.wtf.openspaces.activities.*
import zio.*
import java.time.{LocalDateTime, ZoneOffset}

/** Random activity actions for chaos mode. */
object ActivityChaos:

  /** 40% create, 35% toggle interest, 15% edit own, 10% delete own (falls back to create). */
  def randomAction(
    current: ActivityState,
    allowedWindow: Option[(LocalDateTime, LocalDateTime)],
  ): UIO[ActivityAction] =
    for
      person <- RandomUsers.randomPerson
      roll   <- Random.nextIntBounded(100)
      create = createAction(person, allowedWindow)
      action <- current.activities.values.toList match
        case Nil => create
        case activities =>
          if roll < 40 then create
          else if roll < 75 then
            Random.nextIntBounded(activities.size).map { i =>
              val activity = activities(i)
              ActivityAction.SetInterest(activity.id, person, interested = !activity.hasMember(person))
            }
          else if roll < 90 then
            current.ownedBy(person).headOption match
              case Some(owned) =>
                for
                  desc      <- randomDescription
                  eventTime <- randomEventTime(allowedWindow)
                yield ActivityAction.Update(owned.id, desc, eventTime, person)
              case None => create
          else
            current.ownedBy(person).headOption match
              case Some(owned) => ZIO.succeed(ActivityAction.Delete(owned.id, person))
              case None        => create
    yield action

  private def createAction(
    person: Person,
    allowedWindow: Option[(LocalDateTime, LocalDateTime)],
  ): UIO[ActivityAction] =
    for
      desc      <- randomDescription
      eventTime <- randomEventTime(allowedWindow)
    yield ActivityAction.Create(desc, eventTime, person)

  private def randomEventTime(allowedWindow: Option[(LocalDateTime, LocalDateTime)]): UIO[LocalDateTime] =
    allowedWindow match
      case Some((start, end)) =>
        val secondsRange = java.time.Duration.between(start, end).getSeconds.max(0L)
        Random.nextLongBetween(0L, secondsRange + 1L).map(start.plusSeconds)
      case None =>
        Clock.instant.map(LocalDateTime.ofInstant(_, ZoneOffset.UTC))

  private val titles = List(
    "Pair on a side project",
    "Coffee walk and architecture chat",
    "Open source bug smash",
    "Mentoring hour",
    "Product teardown session",
    "Design critique huddle",
  )

  private def randomDescription: UIO[ActivityDescription] =
    Random.nextIntBounded(titles.size).map(i => ActivityDescription.unsafeMake(titles(i)))
