package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import neotype.unwrap

import co.wtf.openspaces.*

object LightningTalksView:

  private def nightLabel(night: LightningTalkNight): String =
    night match
      case LightningTalkNight.Monday   => "Monday Night"
      case LightningTalkNight.Tuesday  => "Tuesday Night"
      case LightningTalkNight.Thursday => "Thursday Night"

  private def proposalActions(
    proposal: LightningTalkProposal,
    currentUser: Person,
    isAdmin: Boolean,
    sendLightningAction: LightningTalkAction => Unit,
    setErrorMsg: Observer[Option[String]],
  ): HtmlElement =
    val canManage = isAdmin || proposal.speaker == currentUser
    if !canManage then
      div()
    else
      div(
        cls := "LightningTalk-actions",
        button(
          cls := "LightningTalk-actionButton",
          "Edit",
          onClick --> Observer { _ =>
            val currentTitle = proposal.topicName
            val updatedTitle = dom.window.prompt("Edit lightning talk title", currentTitle)
            Option(updatedTitle).foreach { title =>
              Topic.make(title) match
                case Left(error) =>
                  setErrorMsg.onNext(Some(error))
                case Right(validTopic) =>
                  sendLightningAction(
                    LightningTalkAction.Rename(proposal.id, validTopic),
                  )
            }
          },
        ),
        button(
          cls := "LightningTalk-actionButton LightningTalk-actionButton--danger",
          "Delete",
          onClick --> Observer { _ =>
            if dom.window.confirm("Delete this lightning talk proposal?") then
              sendLightningAction(
                LightningTalkAction.Delete(proposal.id),
              )
          },
        ),
      )

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
            proposalActions(
              proposal,
              currentUser,
              isAdmin,
              sendLightningAction,
              setErrorMsg,
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
                val where = proposal.assignment match
                  case Some(assignment) =>
                    s"${nightLabel(assignment.night)} slot #${assignment.slot.unwrap}"
                  case None =>
                    "Unassigned"
                div(
                  cls := "LightningTalk-row",
                  div(
                    cls := "LightningTalk-main",
                    div(cls := "LightningTalk-title", proposal.topicName),
                    div(
                      cls := "LightningTalk-meta",
                      s"${proposal.speakerName} • $where",
                    ),
                  ),
                  proposalActions(
                    proposal,
                    currentUser,
                    isAdmin,
                    sendLightningAction,
                    setErrorMsg,
                  ),
                )
              },
            )
        },
      ),
    )
