package co.wtf.openspaces.location

import co.wtf.openspaces.Person
import neotype.unwrap
import zio.*

/** In-memory location sharing service.
  * 
  * Locations are ephemeral - not persisted to database.
  * Auto-expires after 2 hours of sharing.
  */
case class LocationService(
  state: Ref[LocationState],
):
  def snapshot: UIO[LocationState] =
    state.get

  def applyConfirmed(action: LocationActionConfirmed): UIO[Unit] =
    state.update(_.applyConfirmed(action))

  def applyAction(action: LocationAction): UIO[LocationActionConfirmed] =
    val now = java.lang.System.currentTimeMillis()
    action match
      case LocationAction.StartSharing(person, lat, lng, accuracy) =>
        val location = SharedLocation.create(
          person = person,
          displayName = None, // TODO: Could look up display name from UserRepository
          latitude = lat,
          longitude = lng,
          accuracy = accuracy,
          now = now,
        )
        val confirmed = LocationActionConfirmed.SharingStarted(location)
        state.update(_.applyConfirmed(confirmed)).as(confirmed)

      case LocationAction.UpdateLocation(person, lat, lng, accuracy) =>
        state.get.flatMap { current =>
          current.locations.get(person) match
            case Some(existing) =>
              // Keep the original expiresAt, just update position
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
              val location = SharedLocation.create(
                person = person,
                displayName = None,
                latitude = lat,
                longitude = lng,
                accuracy = accuracy,
                now = now,
              )
              val confirmed = LocationActionConfirmed.SharingStarted(location)
              state.update(_.applyConfirmed(confirmed)).as(confirmed)
        }

      case LocationAction.StopSharing(person) =>
        val confirmed = LocationActionConfirmed.SharingStopped(person, StopReason.UserStopped)
        state.update(_.applyConfirmed(confirmed)).as(confirmed)

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
    val now = java.lang.System.currentTimeMillis()
    state.get.flatMap { current =>
      val expired = current.locations.values.filter(_.isExpired(now)).toList
      val confirmations = expired.map(loc =>
        LocationActionConfirmed.SharingStopped(loc.person, StopReason.Expired)
      )
      state.update(s => confirmations.foldLeft(s)(_.applyConfirmed(_))).as(confirmations)
    }

object LocationService:
  val layer: ZLayer[Any, Nothing, LocationService] =
    ZLayer.fromZIO:
      for
        stateRef <- Ref.make(LocationState.empty)
      yield LocationService(stateRef)
