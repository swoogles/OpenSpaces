package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import neotype.*
import co.wtf.openspaces.{Discussion, DiscussionAction, Feedback, Person, VotePosition, GlyphiconUtils, SvgIcon}
import co.wtf.openspaces.*
import io.laminext.websocket.*

/** Reusable voting buttons component - matches the Topics view styling */
object VoteButtons:
  def apply(
    discussion: Discussion,
    name: StrictSignal[Person],
    topicUpdates: DiscussionAction => Unit,
    connectionStatus: ConnectionStatusManager[WebSocketMessage, WebSocketMessage]
  ) =
    val currentFeedback =
      discussion.interestedParties.find(_.voter == name.now())
    val isInterested =
      currentFeedback.exists(_.position == VotePosition.Interested)
    val isNotInterested =
      currentFeedback.exists(_.position == VotePosition.NotInterested)
    val votes = discussion.votes
    
    def handleVote(target: VotePosition): Unit =
      // Only allow voting if connection is ready (prevents actions during reconnect/sync)
      if connectionStatus.checkReady() then
        val voter = name.now()
        val currentPosition = currentFeedback.map(_.position)
        // Only update if switching to a different position (no going back to neutral)
        if !currentPosition.contains(target) then
          topicUpdates(DiscussionAction.Vote(discussion.id, Feedback(voter, target)))
      else
        println("Connection not ready, ignoring vote action")

    div(
      cls := "VoteButtonRow",
      button(
        cls := "VoteButton",
        cls := "VoteButton--interested",
        if isInterested then cls := "VoteButton--active" else emptyMod,
        onClick --> Observer { _ =>
          handleVote(VotePosition.Interested)
        },
        SvgIcon(GlyphiconUtils.heart, "VoteIcon"),
      ),
      if (currentFeedback.isDefined) {
        span(
          cls := "VoteCount",
          votes.toString,
        )
      } else {
        span(cls := "VoteCount VoteCount--hidden", "?")
      },
      button(
        cls := "VoteButton",
        cls := "VoteButton--notinterested",
        if isNotInterested then cls := "VoteButton--active" else emptyMod,
        onClick --> Observer { _ =>
          handleVote(VotePosition.NotInterested)
        },
        SvgIcon(GlyphiconUtils.noSymbol, "VoteIcon"),
      ),
    )
