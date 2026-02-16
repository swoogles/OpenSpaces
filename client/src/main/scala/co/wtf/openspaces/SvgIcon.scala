package co.wtf.openspaces

import com.raquo.laminar.api.L.*
import neotype.unwrap
import co.wtf.openspaces.components.UiClasses

object GitHubAvatar {

  def apply(
    facilitator: Person,
    clsName: String = "",
  ): HtmlElement =
    img(
      cls := UiClasses.join("github-avatar", clsName),
      src := s"https://github.com/${facilitator.unwrap}.png?size=100",
      alt := s"${facilitator.unwrap}'s GitHub avatar",
      title := facilitator.unwrap,
    )

  /** Creates a GitHub avatar with voting state styling and accessibility attributes.
    *
    * Wraps the img in a span so CSS pseudo-elements (like X overlay) can be applied.
    *
    * @param facilitator
    *   The GitHub username of the topic facilitator
    * @param votingState
    *   The user's voting state for the associated topic
    * @param topicName
    *   The topic name for screen reader context
    * @param additionalClasses
    *   Any additional CSS classes to apply
    */
  def withVotingState(
    facilitator: Person,
    votingState: VotingState,
    topicName: String,
    additionalClasses: String = "",
  ): HtmlElement =
    // Wrap in span so ::before/::after pseudo-elements work for X overlay
    span(
      cls := UiClasses.join("avatar-wrapper", votingState.cssClass, additionalClasses),
      img(
        cls := "github-avatar",
        src := s"https://github.com/${facilitator.unwrap}.png?size=100",
        alt := s"${facilitator.unwrap}'s GitHub avatar for $topicName",
        aria.label := s"$topicName - ${votingState.ariaLabel}",
        role := "img",
        title := s"${facilitator.unwrap} - ${votingState.ariaLabel}",
      ),
    )

}

object SvgIcon {

  def apply(
    glyphicon: Glyphicon,
    clsName: String = "",
  ) =
    img(
      cls := UiClasses.join("glyphicon", clsName),
      src := s"/glyphicons/${glyphicon.name}",
    )

  /** Creates an icon with voting state styling and accessibility attributes.
    *
    * @param glyphicon
    *   The icon to display
    * @param votingState
    *   The user's voting state for the associated topic
    * @param topicName
    *   The topic name for screen reader context
    * @param additionalClasses
    *   Any additional CSS classes to apply
    */
  def withVotingState(
    glyphicon: Glyphicon,
    votingState: VotingState,
    topicName: String,
    additionalClasses: String = "",
  ): HtmlElement =
    img(
      cls := UiClasses.join("glyphicon", votingState.cssClass, additionalClasses),
      src := s"/glyphicons/${glyphicon.name}",
      // Accessible label combining topic name and voting state
      aria.label := s"$topicName - ${votingState.ariaLabel}",
      // Role for screen readers to treat as meaningful image
      role := "img",
      // Title for tooltip on hover (helpful for sighted users too)
      title := votingState.ariaLabel,
    )

}
