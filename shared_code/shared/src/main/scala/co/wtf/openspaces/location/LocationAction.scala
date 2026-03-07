package co.wtf.openspaces.location

import co.wtf.openspaces.Person
import neotype.interop.ziojson.given
import zio.json.*

/** Actions sent from client to server */
enum LocationAction derives JsonCodec:
  /** Start sharing location (first update) */
  case StartSharing(
    person: Person,
    latitude: Double,
    longitude: Double,
    accuracy: Double,
  )
  
  /** Update location while sharing */
  case UpdateLocation(
    person: Person,
    latitude: Double,
    longitude: Double,
    accuracy: Double,
  )
  
  /** Stop sharing location */
  case StopSharing(person: Person)

/** Why location sharing stopped */
enum StopReason derives JsonCodec:
  case UserStopped    // Manual toggle off
  case Expired        // Auto-timeout (2 hours)
  case Disconnected   // WebSocket closed

/** Confirmations broadcast from server to all clients */
enum LocationActionConfirmed derives JsonCodec:
  /** User started sharing */
  case SharingStarted(location: SharedLocation)
  
  /** User's location updated */
  case LocationUpdated(location: SharedLocation)
  
  /** User stopped sharing */
  case SharingStopped(person: Person, reason: StopReason)
  
  /** Full state sync (sent on connect) */
  case StateReplace(locations: List[SharedLocation])
