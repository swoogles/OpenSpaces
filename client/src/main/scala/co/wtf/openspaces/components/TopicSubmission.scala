package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}

import co.wtf.openspaces.{DiscussionAction, Person, Topic}

/** Topic submission form component.
  * 
  * Extracted from FrontEnd.scala for better code organization.
  */
object TopicSubmission:
  
  def apply(
    submitEffect: Observer[DiscussionAction],
    name: StrictSignal[Person],
    setErrorMsg: Observer[Option[String]],
  ): HtmlElement =
    val textVar = Var("")
    val isFocused = Var(false)
    
    div(
      cls := "TopicSubmission",
      cls <-- isFocused.signal.map(f => if f then "TopicSubmission--focused" else ""),
      div(
        cls := "TopicSubmission-inputWrapper",
        textArea(
          cls := "TopicSubmission-textArea",
          placeholder := "What topic would you like to discuss?",
          value <-- textVar,
          onInput.mapToValue --> textVar,
          onFocus --> Observer(_ => isFocused.set(true)),
          onBlur --> Observer(_ => isFocused.set(false)),
        ),
      ),
      button(
        cls := "TopicSubmission-button",
        onClick
          .mapTo(textVar.now())
          .map(s =>
            val res = Topic.make(s)
            res match
              case Left(value) =>
                setErrorMsg.onNext(Some(value))
                None
              case Right(value) =>
                Some(value),
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
