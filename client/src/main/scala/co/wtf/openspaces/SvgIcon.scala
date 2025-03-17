package co.wtf.openspaces

import com.raquo.laminar.api.L.*

object SvgIcon {

  def apply(
    name: String,
    clsName: String = "",
  ) =
    img(
      cls := s"glyphicon $clsName",
      src := s"/glyphicons/$name",
    )

}
