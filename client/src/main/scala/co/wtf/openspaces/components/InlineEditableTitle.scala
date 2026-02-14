package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import co.wtf.openspaces.{Discussion, Person}

/** Inline editable title for topic cards.
  * Shows as plain text with an edit button for the facilitator.
  */
object InlineEditableTitle:
  def apply(
    topic: Discussion,
    currentUser: Person,
    onRename: String => Unit,
    onDelete: () => Unit,
  ): HtmlElement =
    val isEditing = Var(false)
    val editValue = Var(topic.topicName)
    val canEdit = topic.facilitator == currentUser
    
    def saveAndClose(): Unit =
      val newValue = editValue.now().trim
      if newValue.nonEmpty && newValue != topic.topicName then
        onRename(newValue)
      isEditing.set(false)
    
    def cancelEdit(): Unit =
      editValue.set(topic.topicName)
      isEditing.set(false)
    
    def startEditing(e: dom.MouseEvent): Unit =
      e.stopPropagation()
      e.preventDefault()
      editValue.set(topic.topicName)
      isEditing.set(true)
    
    div(
      cls := "InlineEditableTitle",
      child <-- isEditing.signal.map { editing =>
        if editing then
          div(
            cls := "InlineEditableTitle-editRow",
            input(
              cls := "InlineEditableTitle-input",
              typ := "text",
              value <-- editValue.signal,
              onInput.mapToValue --> editValue,
              onBlur --> Observer(_ => saveAndClose()),
              onKeyDown --> Observer { (e: dom.KeyboardEvent) =>
                e.key match
                  case "Enter" => saveAndClose()
                  case "Escape" => cancelEdit()
                  case _ => ()
              },
              // Prevent swipe from capturing input interactions
              onMouseDown --> Observer { (e: dom.MouseEvent) => e.stopPropagation() },
              onTouchStart --> Observer { (e: dom.TouchEvent) => e.stopPropagation() },
              onMountCallback { ctx =>
                val el = ctx.thisNode.ref.asInstanceOf[dom.html.Input]
                el.focus()
                // Put cursor at end of text (not selecting all)
                val len = el.value.length
                el.setSelectionRange(len, len)
              },
            ),
            a(
              cls := "InlineEditableTitle-cancelBtn",
              href := "#",
              onClick --> Observer { (e: dom.MouseEvent) =>
                e.preventDefault()
                e.stopPropagation()
                cancelEdit()
              },
              "✕",
            ),
          )
        else
          div(
            cls := "InlineEditableTitle-displayRow",
            span(
              cls := "InlineEditableTitle-text",
              topic.topicName,
            ),
            // Edit and delete buttons only shown for facilitator
            if canEdit then
              span(
                cls := "InlineEditableTitle-actions",
                a(
                  cls := "InlineEditableTitle-editBtn",
                  href := "#",
                  title := "Edit title",
                  onClick --> Observer { (e: dom.MouseEvent) => 
                    e.preventDefault()
                    startEditing(e) 
                  },
                  "✎",
                ),
                a(
                  cls := "InlineEditableTitle-deleteBtn",
                  href := "#",
                  title := "Delete topic",
                  onClick --> Observer { (e: dom.MouseEvent) => 
                    e.preventDefault()
                    e.stopPropagation()
                    if dom.window.confirm(s"Delete topic '${topic.topicName}'? This cannot be undone.") then
                      onDelete()
                  },
                  "🗑",
                ),
              )
            else
              span()
            ,
          )
      },
    )
