package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

import co.wtf.openspaces.*
import neotype.unwrap

object LightningTalkProposalCard:
  def nightLabel(night: LightningTalkNight): String =
    night match
      case LightningTalkNight.Monday   => "Monday Night"
      case LightningTalkNight.Tuesday  => "Tuesday Night"
      case LightningTalkNight.Thursday => "Thursday Night"

  def locationLabel(proposal: LightningTalkProposal): String =
    proposal.assignment match
      case Some(assignment) =>
        s"${nightLabel(assignment.night)} slot #${assignment.slot.unwrap}"
      case None =>
        "Unassigned"

  def apply(
    proposal: LightningTalkProposal,
    metaText: String,
    rowClass: String = "LightningTalk-row",
    slotNumber: Option[Int] = None,
    currentUser: Option[Person] = None,
    isAdmin: Boolean = false,
    sendLightningAction: Option[LightningTalkAction => Unit] = None,
    setErrorMsg: Option[Observer[Option[String]]] = None,
  ): HtmlElement =
    val maybeActions =
      (currentUser, sendLightningAction, setErrorMsg) match
        case (Some(user), Some(sendAction), Some(errorObserver))
            if isAdmin || proposal.speaker == user =>
          Some(
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
                        errorObserver.onNext(Some(error))
                      case Right(validTopic) =>
                        sendAction(LightningTalkAction.Rename(proposal.id, validTopic))
                  }
                },
              ),
              button(
                cls := "LightningTalk-actionButton LightningTalk-actionButton--danger",
                "Delete",
                onClick --> Observer { _ =>
                  if dom.window.confirm("Delete this lightning talk proposal?") then
                    sendAction(LightningTalkAction.Delete(proposal.id))
                },
              ),
            ),
          )
        case _ =>
          None

    div(
      cls := rowClass,
      slotNumber
        .map(slot => div(cls := "LightningTalk-slotNumber", s"#$slot"))
        .getOrElse(emptyNode),
      div(
        cls := "LightningTalk-main",
        div(cls := "LightningTalk-title", proposal.topicName),
        div(
          cls := "LightningTalk-metaRow",
          div(cls := "LightningTalk-meta", metaText),
          proposal.slackThreadUrl match
            case Some(url) =>
              a(
                href := url,
                target := "_blank",
                cls := "SlackThreadLink",
                title := "Discuss in Slack",
                img(src := "/icons/slack.svg", cls := "SlackIcon"),
              )
            case None =>
              emptyNode,
        ),
      ),
      maybeActions.getOrElse(emptyNode),
    )
