package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import scala.concurrent.ExecutionContext.Implicits.global
import neotype.unwrap

import co.wtf.openspaces.{AppState, RandomActionClient, PendingUser, ApprovedUser}
import co.wtf.openspaces.services.AuthService

/** Admin view for managing user authorization.
  * Shows pending users that can be approved and approved users that can be revoked.
  */
object UserManagementView:
  def apply(
    randomActionClient: RandomActionClient,
    onClose: Observer[Unit],
  ): HtmlElement =
    val isLoading = Var(false)
    val errorMessage = Var(Option.empty[String])
    val successMessage = Var(Option.empty[String])

    def approveUser(username: String): Unit =
      if dom.window.confirm(s"Approve user '$username'? They will gain full access to the app.") then
        isLoading.set(true)
        errorMessage.set(None)
        successMessage.set(None)
        val adminUsername = AuthService.getCookie("github_username").getOrElse("")
        randomActionClient.approveUser(adminUsername, username).foreach { result =>
          isLoading.set(false)
          if result.success then
            successMessage.set(Some(result.message))
            // Optimistic local update so admin sees immediate feedback.
            val approvedDisplayName = AppState.pendingUsers.now()
              .find(_.username.equalsIgnoreCase(username))
              .flatMap(_.displayName)
            AppState.pendingUsers.update(_.filterNot(_.username.equalsIgnoreCase(username)))
            AppState.approvedUsers.update { existing =>
              if existing.exists(_.username.equalsIgnoreCase(username)) then existing
              else existing :+ ApprovedUser(username, approvedDisplayName)
            }
            // Refresh auth status to get updated lists
            AuthService.fetchAuthStatus(randomActionClient)
          else
            errorMessage.set(Some(result.message))
        }

    def revokeUser(username: String): Unit =
      if dom.window.confirm(s"Revoke access for user '$username'? They will lose the ability to interact with the app.") then
        isLoading.set(true)
        errorMessage.set(None)
        successMessage.set(None)
        val adminUsername = AuthService.getCookie("github_username").getOrElse("")
        randomActionClient.revokeUser(adminUsername, username).foreach { result =>
          isLoading.set(false)
          if result.success then
            successMessage.set(Some(result.message))
            // Optimistic local update so admin sees immediate feedback.
            val pendingDisplayName = AppState.approvedUsers.now()
              .find(_.username.equalsIgnoreCase(username))
              .flatMap(_.displayName)
            AppState.approvedUsers.update(_.filterNot(_.username.equalsIgnoreCase(username)))
            AppState.pendingUsers.update { existing =>
              if existing.exists(_.username.equalsIgnoreCase(username)) then existing
              else existing :+ PendingUser(username, pendingDisplayName, "")
            }
            // Refresh auth status to get updated lists
            AuthService.fetchAuthStatus(randomActionClient)
          else
            errorMessage.set(Some(result.message))
        }

    div(
      cls := "UserManagementView",
      div(
        cls := "UserManagementView-header",
        h2("User Management"),
        button(
          cls := "UserManagementView-closeBtn",
          "✕",
          onClick.mapTo(()) --> onClose,
        ),
      ),
      // Error/Success messages
      child <-- errorMessage.signal.map {
        case Some(msg) => div(cls := "UserManagementView-error", msg)
        case None => emptyNode
      },
      child <-- successMessage.signal.map {
        case Some(msg) => div(cls := "UserManagementView-success", msg)
        case None => emptyNode
      },
      // Loading indicator
      child <-- isLoading.signal.map { loading =>
        if loading then div(cls := "UserManagementView-loading", "Processing...")
        else emptyNode
      },
      // Pending Users Section
      div(
        cls := "UserManagementView-section",
        h3("Pending Approval"),
        child <-- AppState.pendingUsers.signal.map { users =>
          if users.isEmpty then
            div(cls := "UserManagementView-empty", "No pending users")
          else
            div(
              cls := "UserManagementView-list",
              users.map { user =>
                div(
                  cls := "UserManagementView-userCard UserManagementView-userCard--pending",
                  img(
                    cls := "UserManagementView-avatar",
                    src := s"https://github.com/${user.username}.png?size=40",
                    alt := user.username,
                  ),
                  div(
                    cls := "UserManagementView-userInfo",
                    div(cls := "UserManagementView-username", user.displayName.getOrElse(user.username)),
                    div(cls := "UserManagementView-handle", s"@${user.username}"),
                  ),
                  button(
                    cls := "UserManagementView-approveBtn",
                    "✓ Approve",
                    onClick --> Observer(_ => approveUser(user.username)),
                  ),
                )
              },
            )
        },
      ),
      // Approved Users Section
      div(
        cls := "UserManagementView-section",
        h3("Approved Users"),
        child <-- AppState.approvedUsers.signal.map { users =>
          if users.isEmpty then
            div(cls := "UserManagementView-empty", "No approved users")
          else
            div(
              cls := "UserManagementView-list",
              users.map { user =>
                div(
                  cls := "UserManagementView-userCard UserManagementView-userCard--approved",
                  img(
                    cls := "UserManagementView-avatar",
                    src := s"https://github.com/${user.username}.png?size=40",
                    alt := user.username,
                  ),
                  div(
                    cls := "UserManagementView-userInfo",
                    div(cls := "UserManagementView-username", user.displayName.getOrElse(user.username)),
                    div(cls := "UserManagementView-handle", s"@${user.username}"),
                  ),
                  button(
                    cls := "UserManagementView-revokeBtn",
                    "✗ Revoke",
                    onClick --> Observer(_ => revokeUser(user.username)),
                  ),
                )
              },
            )
        },
      ),
    )
