package co.wtf.openspaces.components

import com.raquo.laminar.api.L.*
import neotype.unwrap

import co.wtf.openspaces.Person

/** Reusable component showing GitHub avatars for interested parties.
  *
  * Shows up to maxVisible avatars, with a "+N" indicator for overflow.
  * Used across topic cards, hackathon projects, and activities.
  */
object InterestedPartyAvatars:

  def apply(
    people: List[Person],
    maxVisible: Int = 5,
  ): HtmlElement =
    val total = people.size
    val overflow = total - maxVisible

    div(
      cls := "InterestedPartyAvatars",
      people.take(maxVisible).map { person =>
        img(
          cls := "InterestedPartyAvatars-avatar",
          src := s"https://github.com/${person.unwrap}.png?size=40",
          alt := person.unwrap,
          title := person.unwrap,
        )
      },
      if overflow > 0 then
        span(cls := "InterestedPartyAvatars-overflow", s"+$overflow")
      else
        emptyNode,
    )
