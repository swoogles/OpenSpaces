package co.wtf.openspaces

enum LightningDrawError:
  case NoOpenNights
  case NightIsNotNextOpen(
    requested: LightningTalkNight,
    nextOpen: LightningTalkNight,
  )

case class LightningNightDrawResult(
  night: LightningTalkNight,
  assignments: List[LightningDrawAssignment],
)

object LightningTalkDraw:

  /** Public API entrypoint: draw for the next available night.
    * This is intended to back /draw-for-next-night.
    */
  def drawForNextNight(
    state: LightningTalkState,
    randomize: List[LightningTalkProposal] => List[LightningTalkProposal],
  ): Either[LightningDrawError, LightningNightDrawResult] =
    state.nextNightWithOpenSlot match
      case None => Left(LightningDrawError.NoOpenNights)
      case Some(nextNight) =>
        drawForNight(state, nextNight, randomize)

  /** Internal API: draw for a specific night, rejecting if it is not the next open night.
    */
  def drawForNight(
    state: LightningTalkState,
    requestedNight: LightningTalkNight,
    randomize: List[LightningTalkProposal] => List[LightningTalkProposal],
  ): Either[LightningDrawError, LightningNightDrawResult] =
    state.nextNightWithOpenSlot match
      case None =>
        Left(LightningDrawError.NoOpenNights)
      case Some(nextNight) if nextNight != requestedNight =>
        Left(LightningDrawError.NightIsNotNextOpen(requestedNight, nextNight))
      case Some(_) =>
        val openSlots = state.openSlotsForNight(requestedNight)
        val unassigned = state.unassignedProposals
        val selected = randomize(unassigned).take(openSlots.size)
        val assignments =
          selected.zip(openSlots).map { case (proposal, slot) =>
            LightningDrawAssignment(
              proposal.id,
              LightningAssignment(requestedNight, slot),
            )
          }
        Right(
          LightningNightDrawResult(
            requestedNight,
            assignments,
          ),
        )
