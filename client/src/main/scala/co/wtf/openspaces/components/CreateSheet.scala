package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import neotype.unwrap

import co.wtf.openspaces.*
import co.wtf.openspaces.AppState.CreateEntityType
import co.wtf.openspaces.activities.*
import co.wtf.openspaces.discussions.DiscussionAction
import co.wtf.openspaces.lighting_talks.LightningTalkAction
import co.wtf.openspaces.hackathon.{HackathonProject, HackathonProjectAction, HackathonProjectState, ProjectTitle}

import scala.scalajs.js

/** Unified bottom sheet for creating any entity type.
  *
  * - Persists across view changes
  * - Animates from bottom of screen
  * - Type selector + appropriate form fields
  * - Dismisses only on explicit close
  */
object CreateSheet:

  @js.native
  private trait ShowPickerCapable extends js.Object:
    def showPicker(): Unit = js.native

  private val dateTimeInputFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm")

  private def nowDateTimeInputValue(): String =
    LocalDateTime.now().format(dateTimeInputFormat)

  private def parseLocalDateTime(raw: String): Option[LocalDateTime] =
    scala.util.Try(LocalDateTime.parse(raw.trim, dateTimeInputFormat)).toOption

  private def showNativePicker(input: dom.html.Input): Unit =
    try
      input.asInstanceOf[ShowPickerCapable].showPicker()
    catch
      case _: Throwable =>
        try input.click()
        catch case _: Throwable => ()
        input.focus()

  def apply(
    name: StrictSignal[Person],
    submitTopic: Observer[DiscussionAction],
    sendActivityAction: ActivityAction => Unit,
    sendLightningTalkAction: LightningTalkAction => Unit,
    sendHackathonAction: HackathonProjectAction => Unit,
    hackathonProjectState: Var[HackathonProjectState],
    connectionStatus: ConnectionStatusUI,
  ): HtmlElement =
    val selectedType: Var[Option[CreateEntityType]] = Var(None)
    val errorMsg: Var[Option[String]] = Var(None)
    
    // Pending hackathon create confirmation (existing project, new title)
    val pendingHackathonCreate: Var[Option[(HackathonProject, ProjectTitle)]] = Var(None)

    // Form field vars (shared where possible)
    val titleText = Var("")
    val descriptionText = Var("")
    val eventTime = Var(nowDateTimeInputValue())
    var timeInputRef: Option[dom.html.Input] = None

    def resetForm(): Unit =
      selectedType.set(None)
      errorMsg.set(None)
      titleText.set("")
      descriptionText.set("")
      eventTime.set(nowDateTimeInputValue())
      pendingHackathonCreate.set(None)

    def close(): Unit =
      resetForm()
      AppState.closeCreateSheet()

    def confirmHackathonCreate(): Unit =
      pendingHackathonCreate.now().foreach { case (leaving, newTitle) =>
        if connectionStatus.checkReady() then
          val currentUser = name.now()
          // Leave current project first, then create new one
          sendHackathonAction(HackathonProjectAction.Leave(leaving.id, currentUser))
          sendHackathonAction(HackathonProjectAction.Create(newTitle, currentUser))
          close()
        else
          errorMsg.set(Some("Reconnecting... please wait and try again."))
      }
      pendingHackathonCreate.set(None)

    def cancelHackathonCreate(): Unit =
      pendingHackathonCreate.set(None)

    def submitForm(entityType: CreateEntityType): Unit =
      if !connectionStatus.checkReady() then
        errorMsg.set(Some("Reconnecting... please wait and try again."))
        return

      entityType match
        case CreateEntityType.Topic =>
          Topic.make(titleText.now().trim) match
            case Left(err) => errorMsg.set(Some(err))
            case Right(topic) =>
              submitTopic.onNext(DiscussionAction.Add(topic, name.now()))
              close()

        case CreateEntityType.Activity =>
          val desc = ActivityDescription.make(descriptionText.now().trim)
          val time = parseLocalDateTime(eventTime.now().trim)
          (desc, time) match
            case (Left(err), _) => errorMsg.set(Some(err))
            case (_, None) => errorMsg.set(Some("Please choose a valid date and time"))
            case (Right(description), Some(dt)) =>
              sendActivityAction(ActivityAction.Create(description, dt, name.now()))
              close()

        case CreateEntityType.LightningTalk =>
          // Lightning talks use SetParticipation to indicate willingness to present
          sendLightningTalkAction(LightningTalkAction.SetParticipation(name.now(), participating = true))
          close()

        case CreateEntityType.HackathonProject =>
          val title = titleText.now().trim
          if title.isEmpty then
            errorMsg.set(Some("Please enter a project name"))
          else
            ProjectTitle.make(title) match
              case Left(err) => errorMsg.set(Some(err))
              case Right(validTitle) =>
                val currentUser = name.now()
                hackathonProjectState.now().personCurrentProject(currentUser) match
                  case Some(existingProject) =>
                    // User already has a project - need confirmation
                    pendingHackathonCreate.set(Some((existingProject, validTitle)))
                  case None =>
                    // Direct create
                    sendHackathonAction(HackathonProjectAction.Create(validTitle, currentUser))
                    close()

    div(
      cls := "CreateSheet",
      cls <-- AppState.createSheetOpen.signal.map {
        case true => "CreateSheet--open"
        case false => ""
      },
      // Backdrop
      div(
        cls := "CreateSheet-backdrop",
        onClick --> Observer(_ => close()),
      ),
      // Sheet content
      div(
        cls := "CreateSheet-content",
        // Handle
        div(cls := "CreateSheet-handle"),
        // Header
        div(
          cls := "CreateSheet-header",
          span(cls := "CreateSheet-title", "Create New"),
          button(
            cls := "CreateSheet-closeBtn",
            onClick --> Observer(_ => close()),
            "✕",
          ),
        ),
        // Type selector (when no type selected yet)
        child <-- selectedType.signal.map {
          case None =>
            div(
              cls := "CreateSheet-typeSelector",
              typeButton("💬", "Topic", CreateEntityType.Topic, selectedType),
              typeButton("📅", "Activity", CreateEntityType.Activity, selectedType),
              typeButton("⚡", "Lightning Talk", CreateEntityType.LightningTalk, selectedType),
              typeButton("🛠", "Hackathon Project", CreateEntityType.HackathonProject, selectedType),
            )
          case Some(_) => emptyNode
        },
        // Form (when type is selected)
        child <-- selectedType.signal.map {
          case Some(entityType) =>
            div(
              cls := "CreateSheet-form",
              // Back button to type selector
              button(
                cls := "CreateSheet-backBtn",
                onClick --> Observer(_ => selectedType.set(None)),
                "← Back",
              ),
              // Entity-specific form
              entityType match
                case CreateEntityType.Topic =>
                  topicForm(titleText, errorMsg, () => submitForm(entityType))
                case CreateEntityType.Activity =>
                  activityForm(descriptionText, eventTime, timeInputRef, errorMsg, () => submitForm(entityType))
                case CreateEntityType.LightningTalk =>
                  lightningTalkForm()
                case CreateEntityType.HackathonProject =>
                  hackathonForm(titleText, errorMsg, () => submitForm(entityType))
              ,
              // Error message
              child.maybe <-- errorMsg.signal.map(_.map { msg =>
                div(cls := "CreateSheet-error", msg)
              }),
              // Submit button
              button(
                cls := "CreateSheet-submitBtn",
                onClick --> Observer(_ => submitForm(entityType)),
                entityType match
                  case CreateEntityType.Topic => "Submit Topic"
                  case CreateEntityType.Activity => "Create Activity"
                  case CreateEntityType.LightningTalk => "Sign Me Up"
                  case CreateEntityType.HackathonProject => "Create Project"
                ,
              ),
            )
          case None => emptyNode
        },
      ),
      // Hackathon project confirmation modal
      child <-- pendingHackathonCreate.signal.map {
        case Some((existingProject, _)) =>
          val isOwner = existingProject.owner == name.now()
          val willDelete = existingProject.members.size <= 1
          val warningText = 
            if willDelete then
              s"Creating a new project will delete '${existingProject.title.unwrap}' since you're the only member."
            else if isOwner then
              s"Creating a new project will transfer ownership of '${existingProject.title.unwrap}' to another member."
            else
              s"Creating a new project will remove you from '${existingProject.title.unwrap}'."
          
          ConfirmationModal(
            titleText = "Leave Current Project?",
            messageText = warningText,
            cancelText = "Cancel",
            confirmText = "Leave & Create",
            onCancel = () => cancelHackathonCreate(),
            onConfirm = () => confirmHackathonCreate(),
          )
        case None => emptyNode
      },
    )

  private def typeButton(
    icon: String,
    label: String,
    entityType: CreateEntityType,
    selectedType: Var[Option[CreateEntityType]],
  ): HtmlElement =
    button(
      cls := "CreateSheet-typeBtn",
      onClick --> Observer(_ => selectedType.set(Some(entityType))),
      span(cls := "CreateSheet-typeIcon", icon),
      span(cls := "CreateSheet-typeLabel", label),
    )

  private def topicForm(
    titleText: Var[String],
    errorMsg: Var[Option[String]],
    onSubmit: () => Unit,
  ): HtmlElement =
    div(
      textArea(
        cls := "CreateSheet-input CreateSheet-textArea",
        placeholder := "What topic would you like to discuss?",
        controlled(
          value <-- titleText.signal,
          onInput.mapToValue --> Observer[String] { v =>
            titleText.set(v)
            errorMsg.set(None)
          },
        ),
        onKeyDown --> Observer { (e: dom.KeyboardEvent) =>
          if e.key == "Enter" && !e.shiftKey then
            e.preventDefault()
            onSubmit()
        },
      ),
    )

  private def activityForm(
    descriptionText: Var[String],
    eventTime: Var[String],
    timeInputRef: Option[dom.html.Input],
    errorMsg: Var[Option[String]],
    onSubmit: () => Unit,
  ): HtmlElement =
    var localTimeInputRef: Option[dom.html.Input] = None
    div(
      input(
        cls := "CreateSheet-input",
        typ := "text",
        placeholder := "Short activity description",
        controlled(
          value <-- descriptionText.signal,
          onInput.mapToValue --> Observer[String] { v =>
            descriptionText.set(v)
            errorMsg.set(None)
          },
        ),
        onKeyDown --> Observer { (e: dom.KeyboardEvent) =>
          if e.key == "Enter" then onSubmit()
        },
      ),
      input(
        cls := "CreateSheet-input",
        typ := "datetime-local",
        onMountCallback(ctx => localTimeInputRef = Some(ctx.thisNode.ref)),
        onFocus --> Observer(_ => localTimeInputRef.foreach(showNativePicker)),
        controlled(
          value <-- eventTime.signal,
          onInput.mapToValue --> eventTime.writer,
        ),
        onKeyDown --> Observer { (e: dom.KeyboardEvent) =>
          if e.key == "Enter" then onSubmit()
        },
      ),
    )

  private def lightningTalkForm(): HtmlElement =
    div(
      cls := "CreateSheet-info",
      p("By signing up, you're indicating you're willing to give a ~5 minute lightning talk."),
      p("Topic/title will be assigned closer to the event."),
    )

  private def hackathonForm(
    titleText: Var[String],
    errorMsg: Var[Option[String]],
    onSubmit: () => Unit,
  ): HtmlElement =
    div(
      input(
        cls := "CreateSheet-input",
        typ := "text",
        placeholder := "Project name",
        controlled(
          value <-- titleText.signal,
          onInput.mapToValue --> Observer[String] { v =>
            titleText.set(v)
            errorMsg.set(None)
          },
        ),
        onKeyDown --> Observer { (e: dom.KeyboardEvent) =>
          if e.key == "Enter" then onSubmit()
        },
      ),
    )
