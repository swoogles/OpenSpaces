package co.wtf.openspaces.util

import org.scalajs.dom

/** Centralized menu positioning - uses viewport dimensions only */
object MenuPositioning:
  private val menuMargin = 10.0
  private val standardMenuMaxWidth = 350.0

  /** Standard positioning for dropdown menus - centered horizontally,
    * near top of screen
    */
  def standardMenuPosition(): (Double, Double) =
    val viewportHeight = dom.window.innerHeight
    val viewportWidth = dom.window.innerWidth

    val menuWidth =
      Math.min(standardMenuMaxWidth, viewportWidth - 2 * menuMargin)

    // Center the menu horizontally
    val x = (viewportWidth - menuWidth) / 2

    // Position near top of screen to ensure visibility
    val y = Math.max(20.0, viewportHeight * 0.05)

    (x, y)
