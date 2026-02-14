package co.wtf.openspaces

import org.scalajs.dom.window
import scala.util.Try

/** Safe localStorage wrapper that gracefully handles private browsing mode
  * and other scenarios where localStorage is unavailable or throws.
  */
object SafeStorage:
  private def storage = Try(window.localStorage).toOption

  /** Get an item from localStorage, returning None on any failure */
  def getItem(key: String): Option[String] =
    storage.flatMap(s => Try(Option(s.getItem(key))).toOption.flatten)

  /** Set an item in localStorage, silently failing if unavailable */
  def setItem(key: String, value: String): Unit =
    storage.foreach(s => Try(s.setItem(key, value)))

  /** Remove an item from localStorage, silently failing if unavailable */
  def removeItem(key: String): Unit =
    storage.foreach(s => Try(s.removeItem(key)))

  /** Check if a key exists and equals the expected value */
  def contains(key: String, value: String): Boolean =
    getItem(key).contains(value)
