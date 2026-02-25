package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}

object ConfirmationModal:
  def apply(
    titleText: String,
    messageText: String,
    cancelText: String,
    confirmText: String,
    onCancel: () => Unit,
    onConfirm: () => Unit,
  ): HtmlElement =
    div(
      cls := "ConfirmationModal-overlay",
      onClick --> Observer(_ => onCancel()),
      div(
        cls := "ConfirmationModal",
        onClick.stopPropagation --> Observer.empty,
        h3(cls := "ConfirmationModal-title", titleText),
        p(cls := "ConfirmationModal-message", messageText),
        div(
          cls := "ConfirmationModal-buttons",
          button(
            cls := "ConfirmationModal-button ConfirmationModal-button--cancel",
            cancelText,
            onClick --> Observer(_ => onCancel()),
          ),
          button(
            cls := "ConfirmationModal-button ConfirmationModal-button--confirm",
            confirmText,
            onClick --> Observer(_ => onConfirm()),
          ),
        ),
      ),
    )
