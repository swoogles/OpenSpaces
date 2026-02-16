package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}

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
    metaText: Option[String],
    rowClass: String = "LightningTalk-row",
    slotNumber: Option[Int] = None,
  ): HtmlElement =
    val speakerMeta = metaText match
      case Some(value) if value.nonEmpty =>
        s"${proposal.speakerName} • $value"
      case _ =>
        proposal.speakerName

    div(
      cls := rowClass,
      slotNumber
        .map(slot => div(cls := "LightningTalk-slotNumber", s"#$slot"))
        .getOrElse(emptyNode),
      div(
        cls := "LightningTalk-main",
        div(cls := "LightningTalk-title", "Lightning Talk"),
        div(
          cls := "LightningTalk-metaRow",
          div(cls := "LightningTalk-meta", speakerMeta),
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
    )
