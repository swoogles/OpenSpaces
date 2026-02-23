package co.wtf.openspaces.discussions

import co.wtf.openspaces.Person

/** Represents the user's voting state for a topic, used for styling.
  *   - VotedFor: User has voted in favor (most visible)
  *   - NotVoted: User has not voted (neutral)
  *   - VotedAgainst: User has voted against (least visible)
  */
enum VotingState:
  case VotedFor, NotVoted, VotedAgainst

  /** Returns the CSS class name for this voting state */ // TODO Does not belong here.
  def cssClass: String = this match
    case VotedFor      => "vote-state-for"
    case NotVoted      => "vote-state-neutral"
    case VotedAgainst  => "vote-state-against"

  /** Returns an accessible label describing this voting state */
  def ariaLabel: String = this match
    case VotedFor      => "You voted for this topic"
    case NotVoted      => "You have not voted on this topic"
    case VotedAgainst  => "You voted against this topic"

object VotingState:
  /** Determines the voting state for a given user and discussion */
  def forUser(
    discussion: Discussion,
    user: Person,
  ): VotingState =
    discussion.interestedParties.find(_.voter == user) match
      case Some(feedback) =>
        feedback.position match
          case VotePosition.Interested    => VotedFor
          case VotePosition.NotInterested => VotedAgainst
      case None => NotVoted