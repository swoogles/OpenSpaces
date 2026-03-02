package co.wtf.openspaces.components.activities

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.scalajs.js

import co.wtf.openspaces.*
import co.wtf.openspaces.activities.*

/** Reusable form for creating new activities.
  * 
  * Used by both ActivitiesView and LinearScheduleView.
  */
object NewActivityForm:
  @js.native
  private trait ShowPickerCapable extends js.Object:
    def showPicker(): Unit = js.native

  private val dateTimeInputFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm")

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

  /** The expanded form for creating a new activity.
    * 
    * @param name Current user
    * @param sendActivityAction Action sender
    * @param setErrorMsg Error message setter
    * @param connectionStatus Connection status checker
    * @param onClose Called when form should close (cancel or successful create)
    * @param compact If true, uses more compact styling for inline use
    */
  def apply(
    name: StrictSignal[Person],
    sendActivityAction: ActivityAction => Unit,
    setErrorMsg: Observer[Option[String]],
    connectionStatus: ConnectionStatusUI,
    onClose: () => Unit,
    compact: Boolean = false,
  ): HtmlElement =
    val newDescription = Var("")
    val newEventTime = Var(nowDateTimeInputValue())
    var timeInputRef: Option[dom.html.Input] = None

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
            onClose()

    div(
      cls := "HackathonProjects-createForm",
      cls := (if compact then "HackathonProjects-createForm--compact" else ""),
      input(
        cls := "HackathonProjects-input",
        typ := "text",
        placeholder := "Short activity description",
        controlled(
          value <-- newDescription.signal,
          onInput.mapToValue --> newDescription.writer,
        ),
        onKeyDown --> Observer { (e: dom.KeyboardEvent) =>
          if e.key == "Enter" then validateAndCreate()
        },
      ),
      input(
        cls := "HackathonProjects-input",
        typ := "datetime-local",
        onMountCallback(ctx => timeInputRef = Some(ctx.thisNode.ref)),
        onFocus --> Observer(_ => timeInputRef.foreach(showNativePicker)),
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
          onClick --> Observer(_ => onClose()),
        ),
      ),
    )
