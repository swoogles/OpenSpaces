package co.wtf.openspaces.location

import co.wtf.openspaces.{Person, UserOps}
import co.wtf.openspaces.db.UserRepository
import neotype.unwrap
import zio.*

/** In-memory location sharing service.
  * 
  * Locations are ephemeral - not persisted to database.
  * Auto-expires after 2 hours of sharing.
  */
case class LocationService(
  state: Ref[LocationState],
  userRepository: UserRepository,
):
  def snapshot: UIO[LocationState] =
    state.get

  def applyConfirmed(action: LocationActionConfirmed): UIO[Unit] =
    state.update(_.applyConfirmed(action))

  def applyAction(action: LocationAction): Task[LocationActionConfirmed] =
    UserOps.nowMillis.flatMap { now =>
      action match
        case LocationAction.StartSharing(person, lat, lng, accuracy) =>
          startSharing(person, lat, lng, accuracy, now)

        case LocationAction.UpdateLocation(person, lat, lng, accuracy) =>
          state.get.flatMap { current =>
            current.locations.get(person) match
              case Some(existing) =>
                // Keep the original expiresAt and displayName, just update position
                val updated = existing.copy(
                  latitude = lat,
                  longitude = lng,
                  accuracy = accuracy,
                  timestamp = now,
                )
                val confirmed = LocationActionConfirmed.LocationUpdated(updated)
                state.update(_.applyConfirmed(confirmed)).as(confirmed)
              case None =>
                // Not currently sharing, treat as StartSharing
                startSharing(person, lat, lng, accuracy, now)
          }

        case LocationAction.StopSharing(person) =>
          val confirmed = LocationActionConfirmed.SharingStopped(person, StopReason.UserStopped)
          state.update(_.applyConfirmed(confirmed)).as(confirmed)
    }

  private def startSharing(
    person: Person,
    lat: Double,
    lng: Double,
    accuracy: Double,
    now: Long,
  ): Task[LocationActionConfirmed] =
    for
      userOpt <- userRepository.findByUsername(person.unwrap)
      location = SharedLocation.create(
        person = person,
        displayName = userOpt.flatMap(_.displayName),
        latitude = lat,
        longitude = lng,
        accuracy = accuracy,
        now = now,
      )
      confirmed = LocationActionConfirmed.SharingStarted(location)
      _ <- state.update(_.applyConfirmed(confirmed))
    yield confirmed

  /** Remove a user's location (e.g., on disconnect) */
  def removeUser(person: Person): UIO[Option[LocationActionConfirmed]] =
    state.get.flatMap { current =>
      if current.locations.contains(person) then
        val confirmed = LocationActionConfirmed.SharingStopped(person, StopReason.Disconnected)
        state.update(_.applyConfirmed(confirmed)).as(Some(confirmed))
      else
        ZIO.none
    }

  /** Check for and remove expired locations. Returns confirmations for any expired. */
  def expireStaleLocations: UIO[List[LocationActionConfirmed]] =
    UserOps.nowMillis.flatMap { now =>
      state.modify { current =>
        val confirmations = current.locations.values
          .filter(_.isExpired(now))
          .toList
          .map(loc => LocationActionConfirmed.SharingStopped(loc.person, StopReason.Expired))
        (confirmations, confirmations.foldLeft(current)(_.applyConfirmed(_)))
      }
    }

object LocationService:
  val layer: ZLayer[UserRepository, Nothing, LocationService] =
    ZLayer.fromZIO:
      for
        stateRef <- Ref.make(LocationState.empty)
        userRepo <- ZIO.service[UserRepository]
      yield LocationService(stateRef, userRepo)
