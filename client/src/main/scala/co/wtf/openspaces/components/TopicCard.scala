package co.wtf.openspaces.components

import animus.*
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

import co.wtf.openspaces.{
  Discussion, DiscussionAction, Person, Topic, TopicId, VotePosition,
  GitHubAvatar
}
import co.wtf.openspaces.AppState
import co.wtf.openspaces.*
import io.laminext.websocket.*

/** Topic card component showing a discussion with vote state, heat level, and inline editing.
  *
  * Extracted from FrontEnd.scala for better code organization.
  */
object TopicCard:
  def apply(
    name: StrictSignal[Person],
    topicUpdates: DiscussionAction => Unit,
    signal: Signal[Option[Discussion]],
    connectionStatus: ConnectionStatusManager[DiscussionActionConfirmed, WebSocketMessage],
    transition: Option[Transition] = None,
    enableSwipe: Boolean = true,
    iconModifiers: Seq[Modifier[HtmlElement]] = Seq.empty,
  ): Signal[HtmlElement] =
    signal.map {
      case Some(topic) =>
        // Heat level based on votes (accessible: uses color + border + icon)
        val votes = topic.votes
        val heatLevel =
          if (votes >= 5) "heat-hot"
          else if (votes >= 3) "heat-warm"
          else if (votes >= 1) "heat-mild"
          else "heat-cold"

        // Background color based on the user's personal vote
        val currentUserFeedback = topic.interestedParties.find(_.voter == name.now())
        val backgroundColorByPosition = currentUserFeedback match
          case Some(feedback) if feedback.position == VotePosition.Interested => "#d4edda"    // green - interested
          case Some(feedback) if feedback.position == VotePosition.NotInterested => "#e2e3e5" // gray - not interested
          case _ => "#f8f9fa"                                                                  // neutral - no vote

        // Determine vote status (both show in same position on left)
        val voteStatus = currentUserFeedback.map(_.position)
        val hasVoted = currentUserFeedback.isDefined

        val cardContent = div(
          cls := s"TopicCard $heatLevel", // Heat level class for visual indicator
          // Celebration animation class when vote is confirmed
          cls <-- AppState.celebratingTopics.signal.map { celebrating =>
            celebrating.get(topic.id) match
              case Some(VotePosition.Interested) => "vote-celebrating vote-celebrating--interested"
              case Some(VotePosition.NotInterested) => "vote-celebrating vote-celebrating--notinterested"
              case None => ""
          },
          backgroundColor := backgroundColorByPosition,
          transition match
            case Some(value) => value.height
            case None        => cls:="not-animating-anymore"
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
            cls := (voteStatus match
              case Some(VotePosition.Interested) => "VoteIndicator VoteIndicator--interested VoteIndicator--visible"
              case Some(VotePosition.NotInterested) => "VoteIndicator VoteIndicator--notinterested VoteIndicator--visible"
              case None => "VoteIndicator"
            ),
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
              },
              () => {
                if connectionStatus.checkReady() then
                  topicUpdates(DiscussionAction.Delete(topic.id))
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
                  cls := "RoomSlot RoomSlot--unscheduled",
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
          SwipeableCard(topic, name, topicUpdates, cardContent, connectionStatus)
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
    connectionStatus: ConnectionStatusManager[DiscussionActionConfirmed, WebSocketMessage],
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
