package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import neotype.*
import co.wtf.openspaces.{Discussion, DiscussionAction, Feedback, Person, VotePosition, connectionStatus, GlyphiconUtils, SvgIcon}

/** Reusable voting buttons component - matches the Topics view styling */
object VoteButtons:
  def apply(
    discussion: Discussion,
    name: StrictSignal[Person],
    topicUpdates: DiscussionAction => Unit,
  ) =
    val currentFeedback =
      discussion.interestedParties.find(_.voter == name.now())
    val isInterested =
      currentFeedback.exists(_.position == VotePosition.Interested)
    val isNotInterested =
      currentFeedback.exists(_.position == VotePosition.NotInterested)
    val votes = discussion.votes
    val heatLevel = 
      if (votes >= 5) "heat-hot"
      else if (votes >= 3) "heat-warm"  
      else if (votes >= 1) "heat-mild"
      else "heat-cold"
    
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
        cls := (
          if isInterested then "VoteButton VoteButton--interested VoteButton--active"
          else "VoteButton VoteButton--interested"
        ),
        onClick --> Observer { _ =>
          handleVote(VotePosition.Interested)
        },
        SvgIcon(GlyphiconUtils.heart, "VoteIcon"),
      ),
      if (currentFeedback.isDefined) {
        span(
          cls := s"VoteCount $heatLevel",
          if (votes >= 5) "🔥 " else if (votes >= 3) "♨️ " else "",
          votes.toString,
        )
      } else {
        span(cls := "VoteCount VoteCount--hidden", "?")
      },
      button(
        cls := (
          if isNotInterested then "VoteButton VoteButton--notinterested VoteButton--active"
          else "VoteButton VoteButton--notinterested"
        ),
        onClick --> Observer { _ =>
          handleVote(VotePosition.NotInterested)
        },
        SvgIcon(GlyphiconUtils.noSymbol, "VoteIcon"),
      ),
    )
