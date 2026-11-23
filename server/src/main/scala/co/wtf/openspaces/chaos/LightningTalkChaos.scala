package co.wtf.openspaces.chaos

import co.wtf.openspaces.RandomUsers
import co.wtf.openspaces.lighting_talks.{LightningTalkAction, LightningTalkState}
import zio.*

/** Random lightning-talk actions for chaos mode: a random user toggles participation. */
object LightningTalkChaos:
  def randomAction(current: LightningTalkState): UIO[LightningTalkAction] =
    RandomUsers.randomPerson.map(person =>
      LightningTalkAction.SetParticipation(
        person,
        participating = current.proposalForSpeaker(person).isEmpty,
      ),
    )
