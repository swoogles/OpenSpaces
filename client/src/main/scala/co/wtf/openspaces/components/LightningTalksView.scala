package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import neotype.unwrap

import co.wtf.openspaces.*

object LightningTalksView:

  def apply(
    lightningTalkState: Var[LightningTalkState],
    name: StrictSignal[Person],
    showAdminControls: Signal[Boolean],
    sendLightningAction: LightningTalkAction => Unit,
    setErrorMsg: Observer[Option[String]],
    connectionStatus: ConnectionStatusManager[WebSocketMessage, WebSocketMessage],
  ): HtmlElement =
    val textVar = Var("")
    val isFocused = Var(false)

    val $myProposal = Signal
      .combine(lightningTalkState.signal, name)
      .map { case (state, user) =>
        state.proposalForSpeaker(user)
      }
    val $viewer = Signal.combine(name, showAdminControls)

    val $proposalRows = lightningTalkState.signal.map { state =>
      state.proposals.values.toList.sortBy(p => (p.createdAtEpochMs, p.id.unwrap))
    }

    div(
      cls := "LightningTalks",
      h3(cls := "TopicSection-title", "Lightning Talks"),
      p(
        cls := "TopicSection-subtitle",
        "One proposal per person. Draw is random and fills the next available night.",
      ),
      child <-- Signal.combine($myProposal, $viewer).map {
        case (Some(proposal), (currentUser, isAdmin)) =>
          div(
            cls := "TopicSubmission",
            div(
              cls := "LightningTalk-myProposalNotice",
              s"You already submitted: \"${proposal.topicName}\". Edit or delete your existing proposal below.",
            ),
            LightningTalkProposalCard(
              proposal = proposal,
              metaText = s"${proposal.speakerName} • ${LightningTalkProposalCard.locationLabel(proposal)}",
              rowClass = "LightningTalk-row LightningTalk-row--myProposal",
              currentUser = Some(currentUser),
              isAdmin = isAdmin,
              sendLightningAction = Some(sendLightningAction),
              setErrorMsg = Some(setErrorMsg),
            ),
          )
        case (None, _) =>
          div(
            cls := "TopicSubmission",
            cls <-- isFocused.signal.map { focused =>
              if focused then "TopicSubmission--focused" else ""
            },
            div(
              cls := "TopicSubmission-inputWrapper",
              textArea(
                cls := "TopicSubmission-textArea",
                placeholder := "Propose your lightning talk title",
                value <-- textVar,
                onInput.mapToValue --> textVar,
                onFocus --> Observer(_ => isFocused.set(true)),
                onBlur --> Observer(_ => isFocused.set(false)),
              ),
            ),
            button(
              cls := "TopicSubmission-button",
              onClick --> Observer { _ =>
                if !connectionStatus.checkReady() then
                  setErrorMsg.onNext(Some("Reconnecting... please wait and try again."))
                else
                  lightningTalkState.now().proposalForSpeaker(name.now()) match
                    case Some(_) =>
                      setErrorMsg.onNext(Some("You can only submit one lightning talk proposal."))
                    case None =>
                      Topic.make(textVar.now()) match
                        case Left(error) =>
                          setErrorMsg.onNext(Some(error))
                        case Right(topic) =>
                          sendLightningAction(
                            LightningTalkAction.Submit(topic, name.now()),
                          )
                          textVar.set("")
              },
              span("Submit Lightning Talk"),
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
      // All proposals section (simple audit view)
      div(
        cls := "LightningTalk-list",
        h4(cls := "TopicSection-title", "All Proposals"),
        child <-- Signal.combine($proposalRows, $viewer).map { case (proposals, (currentUser, isAdmin)) =>
          if proposals.isEmpty then
            div(cls := "TopicSection-empty", "No lightning talk proposals yet.")
          else
            div(
              proposals.map { proposal =>
                LightningTalkProposalCard(
                  proposal = proposal,
                  metaText = s"${proposal.speakerName} • ${LightningTalkProposalCard.locationLabel(proposal)}",
                  currentUser = Some(currentUser),
                  isAdmin = isAdmin,
                  sendLightningAction = Some(sendLightningAction),
                  setErrorMsg = Some(setErrorMsg),
                )
              },
            )
        },
      ),
    )
