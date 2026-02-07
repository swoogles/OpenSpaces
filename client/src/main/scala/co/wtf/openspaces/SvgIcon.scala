package co.wtf.openspaces

import com.raquo.laminar.api.L.*
import neotype.unwrap

object GitHubAvatar {

  def apply(
    facilitator: Person,
    clsName: String = "",
  ): HtmlElement =
    img(
      cls := s"github-avatar $clsName",
      src := s"https://github.com/${facilitator.unwrap}.png?size=100",
      alt := s"${facilitator.unwrap}'s GitHub avatar",
      title := facilitator.unwrap,
    )

  /** Creates a GitHub avatar with voting state styling and accessibility attributes.
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
    val allClasses =
      s"github-avatar ${votingState.cssClass} $additionalClasses".trim
    img(
      cls := allClasses,
      src := s"https://github.com/${facilitator.unwrap}.png?size=100",
      alt := s"${facilitator.unwrap}'s GitHub avatar for $topicName",
      // Accessible label combining topic name and voting state
      aria.label := s"$topicName - ${votingState.ariaLabel}",
      // Role for screen readers to treat as meaningful image
      role := "img",
      // Title for tooltip on hover (helpful for sighted users too)
      title := s"${facilitator.unwrap} - ${votingState.ariaLabel}",
    )

}

object SvgIcon {

  def apply(
    glyphicon: Glyphicon,
    clsName: String = "",
  ) =
    img(
      cls := s"glyphicon $clsName",
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
    val allClasses =
      s"glyphicon ${votingState.cssClass} $additionalClasses".trim
    img(
      cls := allClasses,
      src := s"/glyphicons/${glyphicon.name}",
      // Accessible label combining topic name and voting state
      aria.label := s"$topicName - ${votingState.ariaLabel}",
      // Role for screen readers to treat as meaningful image
      role := "img",
      // Title for tooltip on hover (helpful for sighted users too)
      title := votingState.ariaLabel,
    )

}
