package co.wtf.openspaces

import scala.util.Random

object GlyphiconRandomDemo:
  val names: Array[String] =
    GlyphiconUtils.names

  def randomGlyphicon() =
    SvgIcon(
      names(
        Random.nextInt(names.length)
      )
    )

