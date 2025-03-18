package co.wtf.openspaces

import com.raquo.laminar.api.L.*

object SvgIcon {

  def apply(
    glyphicon: Glyphicon,
    clsName: String = "",
  ) =
    img(
      cls := s"glyphicon $clsName",
      src := s"/glyphicons/${glyphicon.name}",
    )

}
