package co.wtf.openspaces.location

import co.wtf.openspaces.Person
import zio.json.*

/** Client-side and server-side state for location sharing */
case class LocationState(
  locations: Map[Person, SharedLocation]
):
  def applyConfirmed(action: LocationActionConfirmed): LocationState =
    action match
      case LocationActionConfirmed.SharingStarted(location) =>
        copy(locations = locations + (location.person -> location))
      
      case LocationActionConfirmed.LocationUpdated(location) =>
        copy(locations = locations + (location.person -> location))
      
      case LocationActionConfirmed.SharingStopped(person, _) =>
        copy(locations = locations - person)
      
      case LocationActionConfirmed.StateReplace(locs) =>
        copy(locations = locs.map(l => l.person -> l).toMap)

  def sharingCount: Int = locations.size

object LocationState:
  val empty: LocationState = LocationState(Map.empty)
