package co.wtf.openspaces.services

import org.scalajs.dom
import scala.scalajs.js

/** Browser Notification API wrapper */
object NotificationService:

  /** Check if notifications are supported */
  def isSupported: Boolean =
    !js.isUndefined(dom.window.asInstanceOf[js.Dynamic].Notification)

  /** Request notification permission */
  def requestPermission(): Unit =
    if !isSupported then
      return
    
    dom.Notification.requestPermission { result =>
      dom.console.log(s"Notification permission: $result")
    }

  /** Check current permission state */
  def permission: String =
    if !isSupported then "denied"
    else dom.Notification.permission

  /** Show a notification that location sharing has expired */
  def showLocationExpiredNotification(): Unit =
    if !isSupported || permission != "granted" then
      return

    val options = js.Dynamic.literal(
      body = "Your location is no longer visible to others. Toggle sharing on to resume.",
      icon = "/images/location-off.png",
      tag = "location-expired",
      requireInteraction = false,
    ).asInstanceOf[dom.NotificationOptions]

    val _ = new dom.Notification("Location Sharing Stopped", options)

  /** Show a generic notification */
  def show(title: String, body: String, tag: Option[String] = None): Unit =
    if !isSupported || permission != "granted" then
      return

    val options = js.Dynamic.literal(
      body = body,
      tag = tag.getOrElse(js.undefined),
    ).asInstanceOf[dom.NotificationOptions]

    val _ = new dom.Notification(title, options)
