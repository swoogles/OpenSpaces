package co.wtf.openspaces.components.activities

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import java.time.LocalDateTime
import java.time.LocalDate
import java.time.LocalTime
import java.time.format.DateTimeFormatter
import neotype.unwrap
import scala.scalajs.js

import co.wtf.openspaces.*
import co.wtf.openspaces.activities.*
import co.wtf.openspaces.components.SwipeableCard
import co.wtf.openspaces.components.lightning_talks.LightningTalksView
import co.wtf.openspaces.lighting_talks.*

object ActivitiesView:
  @js.native
  private trait ShowPickerCapable extends js.Object:
    def showPicker(): Unit = js.native

  private val dateInputFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  private val timeInputFormat = DateTimeFormatter.ofPattern("HH:mm")
  private val displayFormat = DateTimeFormatter.ofPattern("EEE h:mm a")

  def parseLocalDateTime(dateRaw: String, timeRaw: String): Option[LocalDateTime] =
    for
      date <- scala.util.Try(LocalDate.parse(dateRaw.trim, dateInputFormat)).toOption
      time <- scala.util.Try(LocalTime.parse(timeRaw.trim, timeInputFormat)).toOption
    yield LocalDateTime.of(date, time)

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
    val newEventDate = Var("")
    val newEventTime = Var("")
    val showCreateForm = Var(false)
    var createDateInputRef: Option[dom.html.Input] = None
    var createTimeInputRef: Option[dom.html.Input] = None

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
      val eventDateRaw = newEventDate.now().trim
      val eventTimeRaw = newEventTime.now().trim
      val parsedDescription = ActivityDescription.make(descriptionRaw)
      val parsedTime = parseLocalDateTime(eventDateRaw, eventTimeRaw)

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
            newEventTime.set("")
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
              onClick --> Observer(_ => showCreateForm.set(true)),
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
                typ := "date",
                onMountCallback(ctx => createDateInputRef = Some(ctx.thisNode.ref)),
                onFocus --> Observer(_ => createDateInputRef.foreach(showNativePicker)),
                controlled(
                  value <-- newEventDate.signal,
                  onInput.mapToValue --> newEventDate.writer,
                ),
              ),
              button(
                cls := "HackathonProjects-cancelButton",
                "Pick Date",
                onClick --> Observer(_ => createDateInputRef.foreach(showNativePicker)),
              ),
              input(
                cls := "HackathonProjects-input",
                typ := "time",
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
              button(
                cls := "HackathonProjects-cancelButton",
                "Pick Time",
                onClick --> Observer(_ => createTimeInputRef.foreach(showNativePicker)),
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
                    newEventDate.set("")
                    newEventTime.set("")
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
                )
              },
            )
        },
      ),
    )

object ActivityCard:
  enum SwipeAction:
    case Interested, NotInterested

  private val dateInputFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  private val timeInputFormat = DateTimeFormatter.ofPattern("HH:mm")

  def apply(
    activity: Activity,
    currentUser: Person,
    sendActivityAction: ActivityAction => Unit,
    connectionStatus: ConnectionStatusUI,
    setErrorMsg: Observer[Option[String]],
    displayFormat: DateTimeFormatter,
  ): HtmlElement =
    val isInterested = activity.interestedPeople.contains(currentUser)
    val isOwner = activity.creator == currentUser
    val editing = Var(false)
    val editDescription = Var(activity.descriptionText)
    val editEventDate = Var(activity.eventTime.toLocalDate.format(dateInputFormat))
    val editEventTime = Var(activity.eventTime.toLocalTime.format(timeInputFormat))
    var editDateInputRef: Option[dom.html.Input] = None
    var editTimeInputRef: Option[dom.html.Input] = None

    def sendInterest(interested: Boolean): Unit =
      if !connectionStatus.checkReady() then
        setErrorMsg.onNext(Some("Reconnecting... please wait and try again."))
      else
        sendActivityAction(ActivityAction.SetInterest(activity.id, currentUser, interested))

    val cardContent = div(
      cls := "HackathonProjectCard",
      div(
        cls := "HackathonProjectCard-header",
        h4(cls := "HackathonProjectCard-title", activity.descriptionText),
      ),
      div(
        cls := "HackathonProjectCard-members",
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
                  typ := "date",
                  onMountCallback(ctx => editDateInputRef = Some(ctx.thisNode.ref)),
                  onFocus --> Observer(_ => editDateInputRef.foreach(ActivitiesView.showNativePicker)),
                  controlled(
                    value <-- editEventDate.signal,
                    onInput.mapToValue --> editEventDate.writer,
                  ),
                ),
                button(
                  cls := "HackathonProjects-cancelButton",
                  "Pick Date",
                  onClick --> Observer(_ => editDateInputRef.foreach(ActivitiesView.showNativePicker)),
                ),
                input(
                  cls := "HackathonProjects-input",
                  typ := "time",
                  onMountCallback(ctx => editTimeInputRef = Some(ctx.thisNode.ref)),
                  onFocus --> Observer(_ => editTimeInputRef.foreach(ActivitiesView.showNativePicker)),
                  controlled(
                    value <-- editEventTime.signal,
                    onInput.mapToValue --> editEventTime.writer,
                  ),
                ),
                button(
                  cls := "HackathonProjects-cancelButton",
                  "Pick Time",
                  onClick --> Observer(_ => editTimeInputRef.foreach(ActivitiesView.showNativePicker)),
                ),
                div(
                  cls := "HackathonProjects-createFormButtons",
                  button(
                    cls := "HackathonProjects-submitButton",
                    "Save",
                    onClick --> Observer { _ =>
                      val parsedDescription = ActivityDescription.make(editDescription.now().trim)
                      val parsedTime = ActivitiesView.parseLocalDateTime(
                        editEventDate.now().trim,
                        editEventTime.now().trim,
                      )

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
                button(
                  cls := "ConfirmationModal-button ConfirmationModal-button--confirm",
                  "Delete",
                  onClick --> Observer { _ =>
                    if !connectionStatus.checkReady() then
                      setErrorMsg.onNext(Some("Reconnecting... please wait and try again."))
                    else
                      sendActivityAction(ActivityAction.Delete(activity.id, currentUser))
                  },
                ),
              )
          },
        )
      else emptyNode,
      if isInterested then div(cls := "TopicSection-subtitle", "You are interested")
      else emptyNode,
    )

    SwipeableCard[SwipeAction](
      cardContent = cardContent,
      onAction = Observer {
        case SwipeAction.Interested => sendInterest(true)
        case SwipeAction.NotInterested => sendInterest(false)
      },
      leftAction = Some(SwipeableCard.Action(SwipeAction.NotInterested, "👈")),
      rightAction = Some(SwipeableCard.Action(SwipeAction.Interested, "👉")),
    )
