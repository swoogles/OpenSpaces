package co.wtf.openspaces.location

import co.wtf.openspaces.Person
import neotype.*
import neotype.interop.ziojson.given
import zio.json.*

/** A user's shared location, broadcast to all connected clients */
case class SharedLocation(
  person: Person,
  displayName: Option[String],
  latitude: Double,
  longitude: Double,
  accuracy: Double,           // meters
  timestamp: Long,            // epoch ms when location was captured
  expiresAt: Long,            // epoch ms when sharing auto-stops
) derives JsonCodec:
  
  /** Check if this location has expired */
  def isExpired(now: Long): Boolean = now >= expiresAt
  
  /** Minutes remaining until expiry */
  def minutesRemaining(now: Long): Long = 
    Math.max(0, (expiresAt - now) / 60000)

object SharedLocation:
  /** Default sharing duration: 2 hours */
  val DefaultDurationMs: Long = 2 * 60 * 60 * 1000
  
  def create(
    person: Person,
    displayName: Option[String],
    latitude: Double,
    longitude: Double,
    accuracy: Double,
    now: Long,
  ): SharedLocation = SharedLocation(
    person = person,
    displayName = displayName,
    latitude = latitude,
    longitude = longitude,
    accuracy = accuracy,
    timestamp = now,
    expiresAt = now + DefaultDurationMs,
  )
