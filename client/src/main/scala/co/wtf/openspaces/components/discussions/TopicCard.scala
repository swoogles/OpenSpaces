package co.wtf.openspaces.components.discussions

import animus.*
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

import co.wtf.openspaces.{
  Person, Topic, TopicId, GitHubAvatar
}
import co.wtf.openspaces.discussions.VotePosition
import co.wtf.openspaces.discussions.Discussion
import co.wtf.openspaces.discussions.DiscussionAction
import co.wtf.openspaces.discussions.Feedback
import co.wtf.openspaces.AppState
import co.wtf.openspaces.*
import co.wtf.openspaces.components.{InlineEditableTitle, SwipeableCard}
import io.laminext.websocket.*

/** Topic card component showing a discussion with vote state and inline editing.
  *
  * Extracted from FrontEnd.scala for better code organization.
  */
object TopicCard:
  def apply(
    name: StrictSignal[Person],
    topicUpdates: DiscussionAction => Unit,
    signal: Signal[Option[Discussion]],
    connectionStatus: ConnectionStatusUI,
    transition: Option[Transition] = None,
    enableSwipe: Boolean = true,
    iconModifiers: Seq[Modifier[HtmlElement]] = Seq.empty,
  ): Signal[HtmlElement] =
    signal.map {
      case Some(topic) =>
        val votes = topic.votes

        // Background color based on the user's personal vote
        val currentUserFeedback = topic.interestedParties.find(_.voter == name.now())
        val voteBackgroundClass = currentUserFeedback match
          case Some(feedback) if feedback.position == VotePosition.Interested => "TopicCard--interested"
          case Some(feedback) if feedback.position == VotePosition.NotInterested => "TopicCard--notinterested"
          case _ => "TopicCard--neutral"

        // Determine vote status (both show in same position on left)
        val voteStatus = currentUserFeedback.map(_.position)
        val hasVoted = currentUserFeedback.isDefined

        val cardContent = div(
          cls := "TopicCard",
          cls := voteBackgroundClass,
          // Celebration animation class when vote is confirmed
          cls <-- AppState.celebratingTopics.signal.map { celebrating =>
            celebrating.get(topic.id) match
              case Some(VotePosition.Interested) => "vote-celebrating vote-celebrating--interested"
              case Some(VotePosition.NotInterested) => "vote-celebrating vote-celebrating--notinterested"
              case None => ""
          },
          transition match
            case Some(value) => 
              // Use percentage-based translateY for height-relative animation:
              // - Entering cards start 100% above (one full card height) and slide down
              // - Leaving cards slide down 100% (one full card height) into the "Viewed Topics" below
              // This avoids hardcoded pixel values and scales with actual card height
              val $translateY = value.signal.map {
                case TransitionStatus.Inserting => -100.0  // Start one card height above
                case TransitionStatus.Removing => 100.0    // Exit one card height below
                case TransitionStatus.Active => 0.0
              }.spring
              
              Seq(
                value.height,
                transform <-- $translateY.map(pct => s"translateY($pct%)")
              )
            case None => cls:="not-animating-anymore"
          ,
          // Particle elements for celebration effect
          children <-- AppState.celebratingTopics.signal.map { celebrating =>
            if celebrating.contains(topic.id) then
              Seq(
                div(
                  cls := "vote-particles",
                  (1 to 6).map(_ => div(cls := "vote-particle")),
                )
              )
            else Seq.empty
          },
          // Vote status indicator (unified left position) - shows icon + vote count
          div(
            cls := "VoteIndicator",
            voteStatus match
              case Some(VotePosition.Interested) =>
                cls := "VoteIndicator--interested"
              case Some(VotePosition.NotInterested) =>
                cls := "VoteIndicator--notinterested"
              case None => emptyMod
            ,
            if hasVoted then cls := "VoteIndicator--visible" else emptyMod,
            // Icon based on vote type
            span(
              cls := "VoteIndicator-icon",
              voteStatus match
                case Some(VotePosition.Interested) => "♥"
                case Some(VotePosition.NotInterested) => "✗"
                case None => ""
            ),
            // Vote count (only shown after voting)
            if hasVoted then
              span(cls := "VoteIndicator-count", votes.toString)
            else
              span(),
          ),
          div(
            cls := "MainActive",
            // Inline editable topic name (only for the facilitator)
            InlineEditableTitle(
              topic,
              name.now(),
              newTitle => {
                if connectionStatus.checkReady() then
                  Topic.make(newTitle) match
                    case Right(validTopic) =>
                      topicUpdates(DiscussionAction.Rename(topic.id, validTopic))
                    case Left(_) => () // Invalid topic name, ignore
                else
                  connectionStatus.reportError("Syncing latest topics. Please wait a moment.")
              },
              () => {
                if connectionStatus.checkReady() then
                  topicUpdates(DiscussionAction.Delete(topic.id))
                else
                  connectionStatus.reportError("Syncing latest topics. Please wait a moment.")
              }
            ),
          ),
          div(
            cls := "SecondaryActive",
            span(
              GitHubAvatar(topic.facilitator).amend(iconModifiers*),
            ),
            span(
              cls := "FacilitatorName",
              topic.facilitatorName,
            ),
            topic.roomSlot match {
              case Some(roomSlot) =>
                span(
                  cls := "RoomSlot",
                  roomSlot.displayString,
                )
              case None =>
                span(
                  cls := "RoomSlot",
                  cls := "RoomSlot--unscheduled",
                  "Unscheduled",
                )
            },
            topic.slackThreadUrl match {
              case Some(url) =>
                a(
                  href := url,
                  target := "_blank",
                  cls := "SlackThreadLink",
                  title := "Discuss in Slack",
                  img(src := "/icons/slack.svg", cls := "SlackIcon"),
                )
              case None => span()
            },
          ),
        )

        // Wrap with swipe functionality if enabled
        if enableSwipe then
          SwipeableCard(
            cardContent = cardContent,
            onAction = Observer { (position: VotePosition) =>
              if connectionStatus.checkReady() then
                topicUpdates(DiscussionAction.Vote(topic.id, Feedback(name.now(), position)))
              else
                connectionStatus.reportError("Syncing latest topics. Please wait a moment.")
            },
            leftAction = Some(SwipeableCard.Action(VotePosition.NotInterested, "✗")),
            rightAction = Some(SwipeableCard.Action(VotePosition.Interested, "♥")),
          )
        else
          cardContent

      case None =>
        div("nothing")
    }

/** Subview showing a list of topic cards with animation support.
  */
object DiscussionSubview:
  def apply(
    topicsOfInterest: Signal[List[Discussion]],
    votePosition: Option[VotePosition],
    name: StrictSignal[Person],
    topicUpdates: DiscussionAction => Unit,
    updateTargetDiscussion: Observer[Discussion],
    connectionStatus: ConnectionStatusUI,
    firstUnjudgedId: Signal[Option[TopicId]] = Signal.fromValue(None),
    showSwipeHint: Signal[Boolean] = Signal.fromValue(false),
  ): HtmlElement =
    div(
      cls := "TopicsContainer",
      children <--
        topicsOfInterest
          .splitTransition(_.id)(
            (
              index: TopicId,
              topic: Discussion,
              signal: Signal[Discussion],
              transition: Transition,
            ) =>
              // Check if this is the first unjudged card (for wiggle hint)
              val isFirstUnjudged = firstUnjudgedId.map(_.contains(topic.id))
              val shouldWiggle = isFirstUnjudged.combineWith(showSwipeHint).map {
                case (first, hint) => first && hint
              }

              div(
                // Apply wiggle class to the wrapper when needed
                cls <-- shouldWiggle.map(if _ then "SwipeHintWiggle" else ""),
                child <-- TopicCard(
                  name,
                  topicUpdates,
                  signal.map(Some(_)),
                  connectionStatus,
                  Some(transition),
                ),
              ),
          ),
    )
