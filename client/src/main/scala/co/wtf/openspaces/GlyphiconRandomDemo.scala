package co.wtf.openspaces

import scala.util.Random

object GlyphiconRandomDemo:
  private val names: List[Glyphicon] =
    GlyphiconUtils.names

  def randomGlyphicon() =
    SvgIcon(
      names(
        Random.nextInt(names.length),
      ),
    )
