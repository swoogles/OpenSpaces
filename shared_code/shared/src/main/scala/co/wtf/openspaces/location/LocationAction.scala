package co.wtf.openspaces.location

import co.wtf.openspaces.{ActorAction, HasActor, Person}
import neotype.interop.ziojson.given
import zio.json.*

/** Actions sent from client to server */
enum LocationAction extends ActorAction derives JsonCodec:
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

  def actor: Person = this match
    case StartSharing(person, _, _, _)   => person
    case UpdateLocation(person, _, _, _) => person
    case StopSharing(person)             => person

/** Why location sharing stopped */
enum StopReason derives JsonCodec:
  case UserStopped    // Manual toggle off
  case Expired        // Auto-timeout (2 hours)
  case Disconnected   // WebSocket closed

/** Confirmations broadcast from server to all clients */
enum LocationActionConfirmed extends HasActor derives JsonCodec:
  /** User started sharing */
  case SharingStarted(location: SharedLocation)

  /** User's location updated */
  case LocationUpdated(location: SharedLocation)

  /** User stopped sharing */
  case SharingStopped(person: Person, reason: StopReason)

  /** Full state sync (sent on connect) */
  case StateReplace(locations: List[SharedLocation])

  def actor: Option[Person] = this match
    case SharingStarted(location) => Some(location.person)
    case LocationUpdated(location) => Some(location.person)
    case SharingStopped(person, _) => Some(person)
    case StateReplace(_) => None

  def isStateChange: Boolean = this match
    case _: StateReplace => false
    case _               => true
