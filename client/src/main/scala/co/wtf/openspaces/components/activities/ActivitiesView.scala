package co.wtf.openspaces.components.activities

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import neotype.unwrap
import scala.scalajs.js

import co.wtf.openspaces.*
import co.wtf.openspaces.activities.*
import co.wtf.openspaces.components.{InterestedPartyAvatars, LeaveConfirmationModal, SwipeableCard}
import co.wtf.openspaces.components.lightning_talks.LightningTalksView
import co.wtf.openspaces.lighting_talks.*

object ActivitiesView:
  @js.native
  private trait ShowPickerCapable extends js.Object:
    def showPicker(): Unit = js.native

  private val dateTimeInputFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm")
  private val displayFormat = DateTimeFormatter.ofPattern("EEE h:mm a")

  private def nowDateTimeInputValue(): String =
    LocalDateTime.now().format(dateTimeInputFormat)

  def parseLocalDateTime(raw: String): Option[LocalDateTime] =
    scala.util.Try(LocalDateTime.parse(raw.trim, dateTimeInputFormat)).toOption

  def showNativePicker(input: dom.html.Input): Unit =
    try
      input.asInstanceOf[ShowPickerCapable].showPicker()
    catch
      case _: Throwable =>
        try input.click()
        catch case _: Throwable => ()
        input.focus()

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
    val newDescription = Var("")
    val newEventTime = Var(nowDateTimeInputValue())
    val showCreateForm = Var(false)
    var createTimeInputRef: Option[dom.html.Input] = None

    // Activity leave confirmation state
    val pendingLeaveActivity: Var[Option[Activity]] = Var(None)

    def handleLeaveActivity(activity: Activity): Unit =
      pendingLeaveActivity.set(Some(activity))

    def confirmLeaveActivity(): Unit =
      pendingLeaveActivity.now().foreach { activity =>
        if connectionStatus.checkReady() then
          sendActivityAction(ActivityAction.SetInterest(activity.id, name.now(), interested = false))
      }
      pendingLeaveActivity.set(None)

    def cancelLeaveActivity(): Unit =
      pendingLeaveActivity.set(None)

    val $activities = activityState.signal.map { state =>
      state.activities.values.toList.sortBy(activity =>
        (
          activity.eventTime,
          -activity.interestCount,
          activity.createdAtEpochMs,
          activity.id.unwrap,
        ),
      )
    }

    def validateAndCreate(): Unit =
      val descriptionRaw = newDescription.now().trim
      val eventTimeRaw = newEventTime.now().trim
      val parsedDescription = ActivityDescription.make(descriptionRaw)
      val parsedTime = parseLocalDateTime(eventTimeRaw)

      (parsedDescription, parsedTime) match
        case (Left(error), _) =>
          setErrorMsg.onNext(Some(error))
        case (_, None) =>
          setErrorMsg.onNext(Some("Please choose a valid date and time"))
        case (Right(description), Some(eventTime)) =>
          if !connectionStatus.checkReady() then
            setErrorMsg.onNext(Some("Reconnecting... please wait and try again."))
          else
            sendActivityAction(ActivityAction.Create(description, eventTime, name.now()))
            newDescription.set("")
            newEventTime.set(nowDateTimeInputValue())
            showCreateForm.set(false)

    div(
      cls := "ActivitiesView",
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
        h3(cls := "TopicSection-title", "Proposed Activities"),
      ),
      div(
        cls := "HackathonProjects-create",
        child <-- showCreateForm.signal.map {
          case false =>
            button(
              cls := "HackathonProjects-createButton",
              "✨ Propose Activity",
              onClick --> Observer { _ =>
                showCreateForm.set(true)
                newEventTime.set(nowDateTimeInputValue())
              },
            )
          case true =>
            div(
              cls := "HackathonProjects-createForm",
              input(
                cls := "HackathonProjects-input",
                typ := "text",
                placeholder := "Short activity description",
                controlled(
                  value <-- newDescription.signal,
                  onInput.mapToValue --> newDescription.writer,
                ),
              ),
              input(
                cls := "HackathonProjects-input",
                typ := "datetime-local",
                onMountCallback(ctx => createTimeInputRef = Some(ctx.thisNode.ref)),
                onFocus --> Observer(_ => createTimeInputRef.foreach(showNativePicker)),
                controlled(
                  value <-- newEventTime.signal,
                  onInput.mapToValue --> newEventTime.writer,
                ),
                onKeyDown --> Observer { (e: dom.KeyboardEvent) =>
                  if e.key == "Enter" then validateAndCreate()
                },
              ),
              div(
                cls := "HackathonProjects-createFormButtons",
                button(
                  cls := "HackathonProjects-submitButton",
                  "Create",
                  onClick --> Observer(_ => validateAndCreate()),
                ),
                button(
                  cls := "HackathonProjects-cancelButton",
                  "Cancel",
                  onClick --> Observer { _ =>
                    showCreateForm.set(false)
                    newDescription.set("")
                    newEventTime.set(nowDateTimeInputValue())
                  },
                ),
              ),
            )
        },
      ),
      div(
        cls := "HackathonProjects-list",
        child <-- $activities.map { activities =>
          if activities.isEmpty then
            div(cls := "HackathonProjects-empty", "No activities yet. Be the first to propose one.")
          else
            div(
              activities.map { activity =>
                ActivityCard(
                  activity = activity,
                  currentUser = name.now(),
                  sendActivityAction = sendActivityAction,
                  connectionStatus = connectionStatus,
                  setErrorMsg = setErrorMsg,
                  displayFormat = displayFormat,
                  onLeave = Some(handleLeaveActivity),
                )
              },
            )
        },
      ),
      // Leave confirmation modal for activities
      child <-- pendingLeaveActivity.signal.map {
        case Some(activity) =>
          LeaveConfirmationModal(
            entity = activity,
            entityType = LeaveConfirmationModal.EntityType.Activity,
            entityTitle = activity.descriptionText,
            currentUser = name.now(),
            onConfirm = () => confirmLeaveActivity(),
            onCancel = () => cancelLeaveActivity(),
          )
        case None =>
          LeaveConfirmationModal.empty
      },
    )

object ActivityCard:
  enum SwipeAction:
    case Interested, Leave

  private val dateTimeInputFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm")

  def apply(
    activity: Activity,
    currentUser: Person,
    sendActivityAction: ActivityAction => Unit,
    connectionStatus: ConnectionStatusUI,
    setErrorMsg: Observer[Option[String]],
    displayFormat: DateTimeFormatter,
    onLeave: Option[Activity => Unit] = None,
  ): HtmlElement =
    val isInterested = activity.hasMember(currentUser)
    val isOwner = activity.creator == currentUser
    val editing = Var(false)
    val editDescription = Var(activity.descriptionText)
    val editEventTime = Var(activity.eventTime.format(dateTimeInputFormat))
    var editTimeInputRef: Option[dom.html.Input] = None

    def sendInterest(interested: Boolean): Unit =
      if !connectionStatus.checkReady() then
        setErrorMsg.onNext(Some("Reconnecting... please wait and try again."))
      else
        sendActivityAction(ActivityAction.SetInterest(activity.id, currentUser, interested))

    val cardContent = div(
      cls := "HackathonProjectCard",
      cls := (if isInterested then "HackathonProjectCard--interested" else ""),
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
                  onFocus --> Observer(_ => editTimeInputRef.foreach(ActivitiesView.showNativePicker)),
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
                      val parsedTime = ActivitiesView.parseLocalDateTime(editEventTime.now().trim)

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
                                currentUser,
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
                // Delete button removed - use swipe left to leave instead
              )
          },
        )
      else emptyNode,
    )

    // Left swipe: Leave if involved (with confirmation if would delete), otherwise no action
    val leftSwipeAction: Option[SwipeableCard.Action[SwipeAction]] =
      if isInterested && onLeave.isDefined then
        Some(SwipeableCard.Action(SwipeAction.Leave, "👋"))
      else
        None

    SwipeableCard[SwipeAction](
      cardContent = cardContent,
      onAction = Observer {
        case SwipeAction.Interested => sendInterest(true)
        case SwipeAction.Leave =>
          // If would delete or is owner, show confirmation modal
          if activity.wouldBeDeletedIfLeaves(currentUser) || isOwner then
            onLeave.foreach(_(activity))
          else
            // Just leave silently (not owner, others remain)
            sendInterest(false)
      },
      leftAction = leftSwipeAction,
      rightAction = Some(SwipeableCard.Action(SwipeAction.Interested, "👉")),
    )
