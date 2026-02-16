package co.wtf.openspaces.components

object UiClasses:
  def build(
    base: String,
    conditional: (String, Boolean)*,
  ): String =
    (base +: conditional.collect { case (className, true) => className }).mkString(" ")

  def join(parts: String*): String =
    parts.filter(_.nonEmpty).mkString(" ")

  def maybe(className: String, include: Boolean): Option[String] =
    Option.when(include)(className)

