package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}

import co.wtf.openspaces.{DiscussionAction, Person, Topic}
import co.wtf.openspaces.ConnectionStatusUI

/** Topic submission form component.
  * 
  * Extracted from FrontEnd.scala for better code organization.
  */
object TopicSubmission:
  
  def apply(
    submitEffect: Observer[DiscussionAction],
    name: StrictSignal[Person],
    connectionStatus: ConnectionStatusUI,
  ): HtmlElement =
    val textVar = Var("")
    val isFocused = Var(false)
    val validationError = Var(Option.empty[String])
    
    div(
      cls := "TopicSubmission",
      cls <-- isFocused.signal.map { focused =>
        if focused then "TopicSubmission--focused" else ""
      },
      div(
        cls := "TopicSubmission-inputWrapper",
        textArea(
          cls := "TopicSubmission-textArea",
          placeholder := "What topic would you like to discuss?",
          value <-- textVar,
          onInput.mapToValue --> Observer { (value: String) =>
            textVar.set(value)
            validationError.set(None)
          },
          onFocus --> Observer(_ => isFocused.set(true)),
          onBlur --> Observer(_ => isFocused.set(false)),
        ),
      ),
      child.maybe <-- validationError.signal.map(_.map { msg =>
        div(
          cls := "TopicSubmission-inlineError",
          msg,
        )
      }),
      button(
        cls := "TopicSubmission-button",
        onClick
          .mapTo(textVar.now())
          .map(s =>
            val res = Topic.make(s)
            res match
              case Left(value) =>
                validationError.set(Some(value))
                None
              case Right(value) =>
                if connectionStatus.withReady("Reconnecting... please wait and try again.") {
                  ()
                } then
                  validationError.set(None)
                  Some(value)
                else
                  validationError.set(Some("Reconnecting... please wait and try again."))
                  None
          )
          .filter(_.isDefined)
          .map(_.get)
          .map(topicTitle =>
            DiscussionAction.Add(
              topicTitle,
              name.now(),
            ),
          )
          .tapEach { case _ =>
            textVar.set("")
          } --> submitEffect,
        span("Submit Topic"),
      ),
    )
