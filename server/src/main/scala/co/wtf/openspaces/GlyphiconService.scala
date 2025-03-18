package co.wtf.openspaces

import zio._

case class GlyphiconService(availableIcons: Ref[List[Glyphicon]]):

  def getRandomIcon: UIO[Glyphicon] = // TODO Handle exhausted list
    for
      icons <- availableIcons.get
      randomIconIdx <- Random.nextIntBounded(icons.length)
      icon = icons(randomIconIdx)
      _ <- availableIcons.update(_.filterNot(_ == icon))
    yield icon

  def addIcon(icon: Glyphicon): UIO[Unit] =
    availableIcons.update(_ :+ icon)


end GlyphiconService

object GlyphiconService:
  def make =
    for
      ref <- Ref.make(GlyphiconUtils.names)
    yield GlyphiconService(ref)

  val live = ZLayer.fromZIO(make)

//  def getRandomIcon = ZIO.serviceWithZIO[GlyphiconService](_.getRandomIcon)
//  def addIcon(icon: Glyphicon) = ZIO.serviceWithZIO[GlyphiconService](_.addIcon(icon))
