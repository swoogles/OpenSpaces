package co.wtf.openspaces.services

import org.scalajs.dom
import org.scalajs.dom.{Geolocation, Position, PositionError, PositionOptions}
import scala.scalajs.js

import co.wtf.openspaces.{AppState, Person}
import co.wtf.openspaces.location.LocationAction

/** Browser Geolocation API wrapper for location sharing */
object GeolocationService:
  
  // Active watch ID (for stopping)
  private var watchId: Option[Int] = None
  
  // Callback to send location updates over WebSocket
  private var sendLocation: Option[LocationAction => Unit] = None

  /** Check if geolocation is available */
  def isAvailable: Boolean =
    !js.isUndefined(dom.window.navigator.asInstanceOf[js.Dynamic].geolocation)

  /** Check current permission state (if supported) */
  def checkPermission(): scala.concurrent.Future[String] =
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.scalajs.js.Thenable.Implicits._
    
    val permissions = dom.window.navigator.asInstanceOf[js.Dynamic].permissions
    if js.isUndefined(permissions) then
      scala.concurrent.Future.successful("unknown")
    else
      permissions
        .query(js.Dynamic.literal(name = "geolocation"))
        .asInstanceOf[js.Promise[js.Dynamic]]
        .toFuture
        .map(result => result.state.asInstanceOf[String])
        .recover { case _ => "unknown" }

  /** Start sharing location with given callback for sending updates */
  def startSharing(onLocationUpdate: LocationAction => Unit): Unit =
    if !isAvailable then
      dom.console.warn("Geolocation not available")
      return
    
    sendLocation = Some(onLocationUpdate)
    
    val options = js.Dynamic.literal(
      enableHighAccuracy = true,
      timeout = 30000,      // 30 seconds
      maximumAge = 60000,   // Accept cached position up to 1 minute old
    ).asInstanceOf[PositionOptions]
    
    val geolocation = dom.window.navigator.geolocation
    
    // Get initial position immediately
    geolocation.getCurrentPosition(
      onPositionSuccess,
      onPositionError,
      options,
    )
    
    // Then watch for updates (browser handles update frequency)
    val id = geolocation.watchPosition(
      onPositionSuccess,
      onPositionError,
      options,
    )
    
    watchId = Some(id)
    AppState.locationSharingEnabled.set(true)

  /** Stop sharing location */
  def stopSharing(): Unit =
    watchId.foreach { id =>
      dom.window.navigator.geolocation.clearWatch(id)
    }
    watchId = None
    sendLocation = None
    AppState.locationSharingEnabled.set(false)
    AppState.locationExpiresAt.set(None)

  /** Toggle sharing on/off */
  def toggleSharing(onLocationUpdate: LocationAction => Unit, onStop: => Unit): Unit =
    if AppState.locationSharingEnabled.now() then
      stopSharing()
      onStop
    else
      startSharing(onLocationUpdate)

  private def onPositionSuccess(position: Position): Unit =
    val coords = position.coords
    val person = AppState.name.now()
    
    val action = if !AppState.locationSharingEnabled.now() then
      // First position - start sharing
      AppState.locationSharingEnabled.set(true)
      LocationAction.StartSharing(
        person = person,
        latitude = coords.latitude,
        longitude = coords.longitude,
        accuracy = coords.accuracy,
      )
    else
      // Subsequent positions - update
      LocationAction.UpdateLocation(
        person = person,
        latitude = coords.latitude,
        longitude = coords.longitude,
        accuracy = coords.accuracy,
      )
    
    sendLocation.foreach(_(action))

  private def onPositionError(error: PositionError): Unit =
    val message = error.code match
      case 1 => "Location permission denied"
      case 2 => "Location unavailable"
      case 3 => "Location request timed out"
      case _ => s"Unknown error: ${error.message}"
    
    dom.console.error(s"Geolocation error: $message")
    
    // If permission denied, stop trying
    if error.code == 1 then
      stopSharing()
