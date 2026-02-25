package co.wtf.openspaces.components.discussions

import animus.*
import com.raquo.laminar.api.L.{*, given}

import co.wtf.openspaces.{Person, Topic, TopicId}
import co.wtf.openspaces.discussions.VotePosition
import co.wtf.openspaces.discussions.Discussion
import co.wtf.openspaces.discussions.DiscussionAction
import co.wtf.openspaces.discussions.Feedback
import co.wtf.openspaces.AppState
import co.wtf.openspaces.*
import co.wtf.openspaces.components.{ConfirmationModal, InlineEditableTitle, InterestedPartyAvatars, SwipeableCard}
import io.laminext.websocket.*
import neotype.unwrap

/** Topic card component showing a discussion with vote state and inline editing.
  *
  * Extracted from FrontEnd.scala for better code organization.
  */
object TopicCard:
  def apply(
    name: StrictSignal[Person],
    topicUpdates: DiscussionAction => Unit,
    signal: Signal[Option[Discussion]],
    isAdmin: Signal[Boolean],
    connectionStatus: ConnectionStatusUI,
    transition: Option[Transition] = None,
    enableSwipe: Boolean = true,
  ): Signal[HtmlElement] =
    Signal.combine(signal, isAdmin).map {
      case (Some(topic), admin) =>
        val votes = topic.votes
        val pendingFacilitatorLeave = Var(false)

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
          if topic.lockedTimeslot then cls := "TopicCard--locked" else emptyMod,
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
          if admin then
            button(
              cls := "TopicCardLockButton",
              cls := (if topic.lockedTimeslot then "TopicCardLockButton--locked"
                      else "TopicCardLockButton--unlocked"),
              if topic.roomSlot.isEmpty then cls := "TopicCardLockButton--disabled"
              else emptyMod,
              title := (
                if topic.roomSlot.isEmpty then
                  "Schedule topic first to lock its timeslot"
                else if topic.lockedTimeslot then
                  "Unlock timeslot for auto-scheduling"
                else
                  "Lock timeslot from auto-scheduling"
              ),
              disabled := topic.roomSlot.isEmpty,
              onClick.stopPropagation --> Observer { _ =>
                if connectionStatus.checkReady() then
                  topicUpdates(
                    DiscussionAction.SetLockedTimeslot(
                      topic.id,
                      topic.lockedTimeslot,
                      !topic.lockedTimeslot,
                    ),
                  )
                else
                  connectionStatus.reportError("Syncing latest topics. Please wait a moment.")
              },
              if topic.lockedTimeslot then "🔒" else "🔓",
            )
          else
            span(),
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
            ),
          ),
          div(
            cls := "SecondaryActive",
            InterestedPartyAvatars(
              topic.interestedParties
                .filter(_.position == VotePosition.Interested)
                .toList
                .sortBy(_.firstVotedAtEpochMs.getOrElse(Long.MaxValue))
                .map(_.voter)
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

        def sendVote(position: VotePosition): Unit =
          if connectionStatus.checkReady() then
            topicUpdates(DiscussionAction.Vote(topic.id, Feedback(name.now(), position)))
          else
            connectionStatus.reportError("Syncing latest topics. Please wait a moment.")

        def handleSwipeVote(position: VotePosition): Unit =
          val currentUser = name.now()
          val requiresConfirmation =
            position == VotePosition.NotInterested && topic.isFacilitator(currentUser)
          if requiresConfirmation then
            pendingFacilitatorLeave.set(true)
          else
            sendVote(position)

        def cancelFacilitatorLeave(): Unit =
          pendingFacilitatorLeave.set(false)

        def confirmFacilitatorLeave(): Unit =
          pendingFacilitatorLeave.set(false)
          sendVote(VotePosition.NotInterested)

        val swipeableContent =
          if enableSwipe then
            SwipeableCard(
              cardContent = cardContent,
              onAction = Observer(handleSwipeVote),
              leftAction = Some(SwipeableCard.Action(VotePosition.NotInterested, "✗")),
              rightAction = Some(SwipeableCard.Action(VotePosition.Interested, "♥")),
            )
          else
            cardContent

        div(
          swipeableContent,
          child <-- pendingFacilitatorLeave.signal.map {
            case true =>
              val currentUser = name.now()
              val wouldDelete = topic.wouldBeDeletedIfFacilitatorLeaves(currentUser)
              val nextFacilitator = topic.nextFacilitator.map(_.unwrap).getOrElse("someone else")
              val title =
                if wouldDelete then s"Delete '${topic.topicName}'?"
                else s"Leave '${topic.topicName}'?"
              val warningText =
                if wouldDelete then
                  "This topic will be deleted since no one else is interested."
                else
                  s"You'll hand facilitation to $nextFacilitator."
              val confirmText =
                if wouldDelete then "Delete Topic"
                else "Transfer & Leave"

              ConfirmationModal(
                titleText = title,
                messageText = warningText,
                cancelText = "Cancel",
                confirmText = confirmText,
                onCancel = () => cancelFacilitatorLeave(),
                onConfirm = () => confirmFacilitatorLeave(),
              )
            case false =>
              emptyNode
          },
        )

      case (None, _) =>
        div("nothing")
    }

/** Subview showing a list of topic cards with animation support.
  */
object DiscussionSubview:
  def apply(
    topicsOfInterest: Signal[List[Discussion]],
    votePosition: Option[VotePosition],
    name: StrictSignal[Person],
    isAdmin: Signal[Boolean],
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
                  isAdmin,
                  connectionStatus,
                  Some(transition),
                ),
              ),
          ),
    )
