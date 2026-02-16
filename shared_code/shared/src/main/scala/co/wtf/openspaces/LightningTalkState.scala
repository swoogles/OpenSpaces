package co.wtf.openspaces

import neotype.unwrap

case class LightningTalkState(
  proposals: Map[LightningTalkId, LightningTalkProposal],
):
  def proposalForSpeaker(
    speaker: Person,
  ): Option[LightningTalkProposal] =
    proposals.values.find(_.speaker == speaker)

  def speakerHasProposal(
    speaker: Person,
  ): Boolean =
    proposalForSpeaker(speaker).isDefined

  def assignmentOccupied(
    assignment: LightningAssignment,
    excludingProposalId: Option[LightningTalkId] = None,
  ): Boolean =
    proposals.values.exists { proposal =>
      !excludingProposalId.contains(proposal.id) &&
      proposal.assignment.contains(assignment)
    }

  def openSlotsForNight(
    night: LightningTalkNight,
  ): List[LightningTalkSlot] =
    val occupiedSlots =
      proposals.values
        .flatMap(_.assignment)
        .filter(_.night == night)
        .map(_.slot)
        .toSet
    (1 to 10)
      .toList
      .flatMap(slotNumber => LightningTalkSlot.make(slotNumber).toOption)
      .filterNot(occupiedSlots.contains)

  def nextNightWithOpenSlot: Option[LightningTalkNight] =
    LightningTalkNight.ordered.find(night => openSlotsForNight(night).nonEmpty)

  def allSlotsFilled: Boolean =
    nextNightWithOpenSlot.isEmpty

  def isNextNightWithOpenSlot(
    night: LightningTalkNight,
  ): Boolean =
    nextNightWithOpenSlot.contains(night)

  def nextOpenAssignment: Option[LightningAssignment] =
    nextNightWithOpenSlot.flatMap(night =>
      openSlotsForNight(night).headOption.map(slot => LightningAssignment(night, slot)),
    )

  def unassignedProposals: List[LightningTalkProposal] =
    proposals.values
      .filter(_.assignment.isEmpty)
      .toList
      .sortBy(proposal => (proposal.createdAtEpochMs, proposal.id.unwrap))

  def assignedForNight(
    night: LightningTalkNight,
  ): List[LightningTalkProposal] =
    proposals.values
      .filter(_.assignment.exists(_.night == night))
      .toList
      .sortBy(proposal =>
        (
          proposal.assignment.map(_.slot.unwrap).getOrElse(Int.MaxValue),
          proposal.createdAtEpochMs,
          proposal.id.unwrap,
        ),
      )

  def apply(
    proposal: LightningTalkProposal,
  ): LightningTalkState =
    copy(proposals = proposals + (proposal.id -> proposal))

  def apply(
    action: LightningTalkActionConfirmed,
  ): LightningTalkState =
    action match
      case LightningTalkActionConfirmed.StateReplace(values) =>
        copy(
          proposals = values.map(value => value.id -> value).toMap,
        )
      case other =>
        copy(proposals = other match
          case LightningTalkActionConfirmed.AddResult(proposal) =>
            proposals + (proposal.id -> proposal)
          case LightningTalkActionConfirmed.SlackThreadLinked(
                proposalId,
                slackThreadUrl,
              ) =>
            proposals.updatedWith(proposalId) {
              _.map(value => value.copy(slackThreadUrl = Some(slackThreadUrl)))
            }
          case LightningTalkActionConfirmed.Delete(proposalId) =>
            proposals - proposalId
          case LightningTalkActionConfirmed.Rename(proposalId, newTopic) =>
            proposals.updatedWith(proposalId) {
              _.map(value => value.copy(topic = newTopic))
            }
          case LightningTalkActionConfirmed.SetAssignment(
                proposalId,
                newAssignment,
              ) =>
            proposals.updatedWith(proposalId) {
              _.map(value => value.copy(assignment = newAssignment))
            }
          case LightningTalkActionConfirmed.DrawForNightResult(
                _,
                assignments,
              ) =>
            assignments.foldLeft(proposals) { (acc, assignment) =>
              acc.updatedWith(assignment.proposalId) {
                _.map(value => value.copy(assignment = Some(assignment.assignment)))
              }
            }
          case LightningTalkActionConfirmed.Unauthorized(_) =>
            proposals
          case LightningTalkActionConfirmed.Rejected(_) =>
            proposals
          case LightningTalkActionConfirmed.StateReplace(_) =>
            proposals,
        )

object LightningTalkState:
  val empty: LightningTalkState =
    LightningTalkState(Map.empty)
