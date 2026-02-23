package co.wtf.openspaces.components.lightning_talks

import com.raquo.laminar.api.L.{*, given}
import neotype.unwrap

import co.wtf.openspaces.*
import co.wtf.openspaces.lighting_talks.*
import co.wtf.openspaces.components.lightning_talks.LightningTalkProposalCard

object LightningTalksView:

  def apply(
    lightningTalkState: Var[LightningTalkState],
    name: StrictSignal[Person],
    showAdminControls: Signal[Boolean],
    sendLightningAction: LightningTalkAction => Unit,
    setErrorMsg: Observer[Option[String]],
    connectionStatus: ConnectionStatusUI,
  ): HtmlElement =
    val $myProposal = Signal
      .combine(lightningTalkState.signal, name)
      .map { case (state, user) =>
        state.proposalForSpeaker(user)
      }

    val $proposalRows = lightningTalkState.signal.map { state =>
      state.proposals.values.toList.sortBy(p => (p.createdAtEpochMs, p.id.unwrap))
    }

    div(
      cls := "LightningTalks",
      child <-- $myProposal.map {
        case Some(proposal) =>
          div(
            cls := "TopicSubmission",
            LightningTalkProposalCard(
              proposal = proposal,
              metaText = Some(LightningTalkProposalCard.locationLabel(proposal)),
              rowClass = "LightningTalk-row LightningTalk-row--myProposal",
            ),
            span("Spots will be randomly assigned...sometime. TBD."),
            button(
              cls := "TopicSubmission-button",
              onClick --> Observer { _ =>
                if !connectionStatus.checkReady() then
                  setErrorMsg.onNext(Some("Reconnecting... please wait and try again."))
                else
                  sendLightningAction(
                    LightningTalkAction.SetParticipation(
                      name.now(),
                      participating = false,
                    ),
                  )
              },
              span("Nevermind, I would prefer to not give a lightning talk."),
            ),
          )
        case None =>
          div(
            cls := "TopicSubmission",
            button(
              cls := "TopicSubmission-button",
              onClick --> Observer { _ =>
                if !connectionStatus.checkReady() then
                  setErrorMsg.onNext(Some("Reconnecting... please wait and try again."))
                else
                  sendLightningAction(
                    LightningTalkAction.SetParticipation(
                      name.now(),
                      participating = true,
                    ),
                  )
              },
              span("I'm willing to give a lightning talk"),
            ),
          )
      },
      child.maybe <-- showAdminControls.map { show =>
        Option.when(show)(
          div(
            cls := "LightningTalk-adminControls",
            button(
              cls := "AdminControls-button AdminControls-button--primary",
              "Draw For Next Night",
              onClick --> Observer { _ =>
                sendLightningAction(LightningTalkAction.DrawForNextNight)
              },
            ),
          ),
        )
      },
      // All proposals section (simple audit view). TODO Restore as Admin only maybe?
      // div(
      //   cls := "LightningTalk-list",
      //   h4(cls := "TopicSection-title", "All Interested Parties"),
      //   child <-- $proposalRows.map { proposals =>
      //     if proposals.isEmpty then
      //       div(cls := "TopicSection-empty", "No lightning talk proposals yet.")
      //     else
      //       div(
      //         proposals.map { proposal =>
      //           LightningTalkProposalCard(
      //             proposal = proposal,
      //             metaText = Some(LightningTalkProposalCard.locationLabel(proposal)),
      //           )
      //         },
      //       )
      //   },
      // ),
    )
