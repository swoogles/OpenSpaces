package co.wtf.openspaces.components.activities

import animus.*
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import neotype.unwrap
import scala.scalajs.js

import co.wtf.openspaces.*
import co.wtf.openspaces.activities.*
import co.wtf.openspaces.components.{ConfirmationModal, InterestedPartyAvatars, SwipeableCard, VotingQueueView}
import co.wtf.openspaces.components.VotableInstances.given
import co.wtf.openspaces.components.lightning_talks.LightningTalksView
import co.wtf.openspaces.lighting_talks.*

object ActivitiesView:
  private val displayFormat = DateTimeFormatter.ofPattern("EEE h:mm a")

  def apply(
    lightningTalkState: Var[LightningTalkState],
    activityState: Var[ActivityState],
    name: StrictSignal[Person],
    showAdminControls: Signal[Boolean],
    sendLightningAction: LightningTalkAction => Unit,
    sendActivityAction: ActivityAction => Unit,
    setErrorMsg: Observer[Option[String]],
    connectionStatus: ConnectionStatusUI,
  ): HtmlElement =
    val $activities = activityState.signal.map { state =>
      state.activities.values.toList
    }

    def renderActivityCard(
      activity: Activity,
      signal: Signal[Activity],
      transition: Option[Transition],
    ): HtmlElement =
      ActivityCard(
        activitySignal = signal,
        currentUser = name,
        sendActivityAction = sendActivityAction,
        connectionStatus = connectionStatus,
        setErrorMsg = setErrorMsg,
        displayFormat = displayFormat,
        transition = transition,
      )

    div(
      cls := "ActivitiesView",
      // Location sharing map at the top
      LocationMapView(),
      LightningTalksView(
        lightningTalkState = lightningTalkState,
        name = name,
        showAdminControls = showAdminControls,
        sendLightningAction = sendLightningAction,
        setErrorMsg = setErrorMsg,
        connectionStatus = connectionStatus,
      ),
      div(
        cls := "ActivitiesView-divider",
      ),
      VotingQueueView[Activity, ActivityId](
        items = $activities,
        currentUser = name,
        renderCard = renderActivityCard,
        renderViewedCard = renderActivityCard,
        queueTitle = count =>
          if count == 1 then "1 activity needs your feedback!"
          else s"$count activities need your feedback!",
        queueSubtitle = "Swipe right if you're interested, left if not. Vote to reveal the next activity.",
        emptyQueueMessage = "You've seen all activities! Check back later for new ones.",
        viewedTitle = "Viewed Activities",
        showSwipeHint = AppState.showSwipeHint.signal,
      ),
    )

object ActivityCard:
  enum SwipeAction:
    case Interested, NotInterested

  private val dateTimeInputFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm")

  /** Reactive version - for VotingQueueView where activity updates via Signal */
  def apply(
    activitySignal: Signal[Activity],
    currentUser: StrictSignal[Person],
    sendActivityAction: ActivityAction => Unit,
    connectionStatus: ConnectionStatusUI,
    setErrorMsg: Observer[Option[String]],
    displayFormat: DateTimeFormatter,
    transition: Option[Transition] = None,
  ): HtmlElement =
    // Wrapper div that reacts to signal changes
    div(
      child <-- activitySignal.map { activity =>
        renderCard(
          activity = activity,
          currentUser = currentUser,
          sendActivityAction = sendActivityAction,
          connectionStatus = connectionStatus,
          setErrorMsg = setErrorMsg,
          displayFormat = displayFormat,
          transition = transition,
        )
      }
    )

  /** Static version - for ScheduleView where activity doesn't need reactive updates */
  def apply(
    activity: Activity,
    currentUser: Person,
    sendActivityAction: ActivityAction => Unit,
    connectionStatus: ConnectionStatusUI,
    setErrorMsg: Observer[Option[String]],
    displayFormat: DateTimeFormatter,
  ): HtmlElement =
    renderCard(
      activity = activity,
      currentUser = Var(currentUser).signal,
      sendActivityAction = sendActivityAction,
      connectionStatus = connectionStatus,
      setErrorMsg = setErrorMsg,
      displayFormat = displayFormat,
      transition = None,
    )

  private def renderCard(
    activity: Activity,
    currentUser: StrictSignal[Person],
    sendActivityAction: ActivityAction => Unit,
    connectionStatus: ConnectionStatusUI,
    setErrorMsg: Observer[Option[String]],
    displayFormat: DateTimeFormatter,
    transition: Option[Transition],
  ): HtmlElement =
    val user = currentUser.now()
    val isInterested = activity.hasMember(user)
    val isOwner = activity.creator == user
    val editing = Var(false)
    val pendingOwnerLeave = Var(false)
    val editDescription = Var(activity.descriptionText)
    val editEventTime = Var(activity.eventTime.format(dateTimeInputFormat))
    var editTimeInputRef: Option[dom.html.Input] = None

    def sendInterest(interested: Boolean): Unit =
      if !connectionStatus.checkReady() then
        setErrorMsg.onNext(Some("Reconnecting... please wait and try again."))
      else
        sendActivityAction(ActivityAction.SetInterest(activity.id, user, interested))

    val cardContent = div(
      cls := "HackathonProjectCard",
      cls := (if isInterested then "HackathonProjectCard--interested" else ""),
      transition match
        case Some(value) =>
          val $translateY = value.signal.map {
            case TransitionStatus.Inserting => -100.0
            case TransitionStatus.Removing => 100.0
            case TransitionStatus.Active => 0.0
          }.spring

          Seq(
            value.height,
            transform <-- $translateY.map(pct => s"translateY($pct%)")
          )
        case None => emptyMod
      ,
      div(
        cls := "HackathonProjectCard-header",
        h4(cls := "HackathonProjectCard-title", activity.descriptionText),
      ),
      div(
        cls := "HackathonProjectCard-members",
        InterestedPartyAvatars(activity.members.map(_.person)),
        span(cls := "HackathonProjectCard-memberCount", s"${activity.interestCount} interested"),
        span(cls := "HackathonProjectCard-memberCount", activity.eventTime.format(displayFormat)),
      ),
      div(
        cls := "LightningTalk-metaRow",
        div(cls := "LightningTalk-meta", s"By ${activity.creatorName}"),
        activity.slackThreadUrl match
          case Some(url) =>
            a(
              href := url,
              target := "_blank",
              cls := "SlackThreadLink",
              title := "Discuss in Slack",
              img(src := "/icons/slack.svg", cls := "SlackIcon"),
              child <-- AppState.slackReplyCounts.signal.map { counts =>
                counts.activities.get(activity.id.unwrap.toString) match {
                  case Some(count) if count > 0 =>
                    span(cls := "SlackReplyCount", count.toString)
                  case _ => emptyNode
                }
              },
            )
          case None =>
            emptyNode,
      ),
      if isOwner then
        div(
          cls := "HackathonProjects-createFormButtons",
          child <-- editing.signal.map { isEditing =>
            if isEditing then
              div(
                input(
                  cls := "HackathonProjects-input",
                  typ := "text",
                  controlled(
                    value <-- editDescription.signal,
                    onInput.mapToValue --> editDescription.writer,
                  ),
                ),
                input(
                  cls := "HackathonProjects-input",
                  typ := "datetime-local",
                  onMountCallback(ctx => editTimeInputRef = Some(ctx.thisNode.ref)),
                  onFocus --> Observer(_ => editTimeInputRef.foreach(NewActivityForm.showNativePicker)),
                  controlled(
                    value <-- editEventTime.signal,
                    onInput.mapToValue --> editEventTime.writer,
                  ),
                ),
                div(
                  cls := "HackathonProjects-createFormButtons",
                  button(
                    cls := "HackathonProjects-submitButton",
                    "Save",
                    onClick --> Observer { _ =>
                      val parsedDescription = ActivityDescription.make(editDescription.now().trim)
                      val parsedTime = NewActivityForm.parseLocalDateTime(editEventTime.now().trim)

                      (parsedDescription, parsedTime) match
                        case (Left(error), _) =>
                          setErrorMsg.onNext(Some(error))
                        case (_, None) =>
                          setErrorMsg.onNext(Some("Please choose a valid date and time"))
                        case (Right(description), Some(eventTime)) =>
                          if !connectionStatus.checkReady() then
                            setErrorMsg.onNext(Some("Reconnecting... please wait and try again."))
                          else
                            sendActivityAction(
                              ActivityAction.Update(
                                activity.id,
                                description,
                                eventTime,
                                user,
                              ),
                            )
                            editing.set(false)
                    },
                  ),
                  button(
                    cls := "HackathonProjects-cancelButton",
                    "Cancel",
                    onClick --> Observer(_ => editing.set(false)),
                  ),
                ),
              )
            else
              div(
                button(
                  cls := "ConfirmationModal-button ConfirmationModal-button--confirm",
                  "Edit",
                  onClick --> Observer(_ => editing.set(true)),
                ),
              )
          },
        )
      else emptyNode,
    )

    def cancelOwnerLeave(): Unit =
      pendingOwnerLeave.set(false)

    def confirmOwnerLeave(): Unit =
      pendingOwnerLeave.set(false)
      sendInterest(false)

    div(
      SwipeableCard[SwipeAction](
        cardContent = cardContent,
        onAction = Observer {
          case SwipeAction.Interested =>
            sendInterest(true)
          case SwipeAction.NotInterested =>
            if isOwner && isInterested then
              pendingOwnerLeave.set(true)
            else
              sendInterest(false)
        },
        leftAction = Some(SwipeableCard.Action(SwipeAction.NotInterested, "👈")),
        rightAction = Some(SwipeableCard.Action(SwipeAction.Interested, "👉")),
      ),
      child <-- pendingOwnerLeave.signal.map {
        case true =>
          val wouldDelete = activity.wouldBeDeletedIfLeaves(user)
          val nextOwner = activity.nextOwner.map(_.unwrap).getOrElse("someone else")
          val title =
            if wouldDelete then s"Delete '${activity.descriptionText}'?"
            else s"Leave '${activity.descriptionText}'?"
          val warningText =
            if wouldDelete then
              "This activity will be deleted since no one else is interested."
            else
              s"You'll hand ownership to $nextOwner."
          val confirmText =
            if wouldDelete then "Delete Activity"
            else "Transfer & Leave"

          ConfirmationModal(
            titleText = title,
            messageText = warningText,
            cancelText = "Cancel",
            confirmText = confirmText,
            onCancel = () => cancelOwnerLeave(),
            onConfirm = () => confirmOwnerLeave(),
          )
        case false =>
          emptyNode
      },
    )
