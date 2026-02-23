package co.wtf.openspaces.components.hackathon

import com.raquo.laminar.api.L.{*, given}
import neotype.*
import org.scalajs.dom

import co.wtf.openspaces.*
import co.wtf.openspaces.hackathon.*

/** Hackathon Projects view for Wednesday hackday.
  * 
  * Shows the user's current project (if any) at the top,
  * followed by a list of other projects they can join.
  * Supports creating new projects and seamless transitions between them.
  */
object HackathonProjectsView:

  def apply(
    hackathonProjectState: Var[HackathonProjectState],
    name: StrictSignal[Person],
    sendHackathonAction: HackathonProjectAction => Unit,
    setErrorMsg: Observer[Option[String]],
    connectionStatus: ConnectionStatusUI,
  ): HtmlElement =
    // User's current project
    val $myProject: Signal[Option[HackathonProject]] = Signal
      .combine(hackathonProjectState.signal, name)
      .map { case (state, user) =>
        state.personCurrentProject(user)
      }

    // All other projects (sorted by member count, then creation time)
    val $otherProjects: Signal[List[HackathonProject]] = Signal
      .combine(hackathonProjectState.signal, name)
      .map { case (state, user) =>
        state.projectsExcludingPerson(user).sortBy(p => (-p.memberCount, p.createdAtEpochMs))
      }

    // State for project creation form
    val newProjectTitle: Var[String] = Var("")
    val showCreateForm: Var[Boolean] = Var(false)
    
    // State for confirmation modals
    val pendingJoin: Var[Option[(HackathonProject, HackathonProject)]] = Var(None) // (leaving, joining)
    val pendingCreate: Var[Option[(HackathonProject, ProjectTitle)]] = Var(None) // (leaving, newTitle)

    def handleCreateProject(): Unit =
      val title = newProjectTitle.now().trim
      ProjectTitle.make(title) match
        case Right(validTitle) =>
          val currentUser = name.now()
          val currentProject = hackathonProjectState.now().personCurrentProject(currentUser)
          
          currentProject match
            case Some(existing) =>
              // Show confirmation modal
              pendingCreate.set(Some((existing, validTitle)))
            case None =>
              // Direct create
              if !connectionStatus.checkReady() then
                setErrorMsg.onNext(Some("Reconnecting... please wait and try again."))
              else
                sendHackathonAction(HackathonProjectAction.Create(validTitle, currentUser))
                newProjectTitle.set("")
                showCreateForm.set(false)
        case Left(error) =>
          setErrorMsg.onNext(Some(error))
    
    def confirmCreate(): Unit =
      pendingCreate.now().foreach { case (leaving, newTitle) =>
        if !connectionStatus.checkReady() then
          setErrorMsg.onNext(Some("Reconnecting... please wait and try again."))
        else
          val currentUser = name.now()
          // Leave current project first, then create new one
          sendHackathonAction(HackathonProjectAction.Leave(leaving.id, currentUser))
          sendHackathonAction(HackathonProjectAction.Create(newTitle, currentUser))
          newProjectTitle.set("")
          showCreateForm.set(false)
      }
      pendingCreate.set(None)
    
    def cancelCreate(): Unit =
      pendingCreate.set(None)

    def handleJoinProject(project: HackathonProject): Unit =
      val currentUser = name.now()
      val currentProject = hackathonProjectState.now().personCurrentProject(currentUser)
      
      currentProject match
        case Some(existing) =>
          // Show confirmation modal
          pendingJoin.set(Some((existing, project)))
        case None =>
          // Direct join
          if !connectionStatus.checkReady() then
            setErrorMsg.onNext(Some("Reconnecting... please wait and try again."))
          else
            sendHackathonAction(HackathonProjectAction.Join(project.id, currentUser))

    def confirmJoin(): Unit =
      pendingJoin.now().foreach { case (_, joining) =>
        if !connectionStatus.checkReady() then
          setErrorMsg.onNext(Some("Reconnecting... please wait and try again."))
        else
          sendHackathonAction(HackathonProjectAction.Join(joining.id, name.now()))
      }
      pendingJoin.set(None)

    def cancelJoin(): Unit =
      pendingJoin.set(None)

    def handleLeaveProject(project: HackathonProject): Unit =
      if !connectionStatus.checkReady() then
        setErrorMsg.onNext(Some("Reconnecting... please wait and try again."))
      else
        sendHackathonAction(HackathonProjectAction.Leave(project.id, name.now()))

    div(
      cls := "HackathonProjects",
      
      // Confirmation modal for joining
      child <-- pendingJoin.signal.map {
        case Some((leaving, joining)) =>
          val currentUser = name.now()
          val isOwner = leaving.isOwner(currentUser)
          val wouldDelete = leaving.wouldBeDeletedIfLeaves(currentUser)
          
          val warningText = 
            if wouldDelete then
              s"Your project '${leaving.titleText}' will be deleted since no one else has joined."
            else if isOwner then
              val nextOwner = leaving.nextOwner.map(_.unwrap).getOrElse("someone else")
              s"You'll hand ownership of '${leaving.titleText}' to $nextOwner."
            else
              s"You'll leave '${leaving.titleText}'."
          
          div(
            cls := "ConfirmationModal-overlay",
            onClick --> Observer(_ => cancelJoin()),
            div(
              cls := "ConfirmationModal",
              onClick.stopPropagation --> Observer.empty,
              h3(cls := "ConfirmationModal-title", s"Join '${joining.titleText}'?"),
              p(cls := "ConfirmationModal-message", warningText),
              div(
                cls := "ConfirmationModal-buttons",
                button(
                  cls := "open-spaces-button",
                  cls := "ConfirmationModal-button ConfirmationModal-button--cancel",
                  "Cancel",
                  onClick --> Observer(_ => cancelJoin()),
                ),
                button(
                  cls := "open-spaces-button",
                  cls := "ConfirmationModal-button ConfirmationModal-button--confirm",
                  "Switch Project",
                  onClick --> Observer(_ => confirmJoin()),
                ),
              ),
            ),
          )
        case None =>
          div()
      },
      
      // Confirmation modal for creating
      child <-- pendingCreate.signal.map {
        case Some((leaving, newTitle)) =>
          val currentUser = name.now()
          val isOwner = leaving.isOwner(currentUser)
          val wouldDelete = leaving.wouldBeDeletedIfLeaves(currentUser)
          
          val warningText = 
            if wouldDelete then
              s"Your project '${leaving.titleText}' will be deleted since no one else has joined."
            else if isOwner then
              val nextOwner = leaving.nextOwner.map(_.unwrap).getOrElse("someone else")
              s"You'll hand ownership of '${leaving.titleText}' to $nextOwner."
            else
              s"You'll leave '${leaving.titleText}'."
          
          div(
            cls := "ConfirmationModal-overlay",
            onClick --> Observer(_ => cancelCreate()),
            div(
              cls := "ConfirmationModal",
              onClick.stopPropagation --> Observer.empty,
              h3(cls := "ConfirmationModal-title", s"Create '${newTitle.unwrap}'?"),
              p(cls := "ConfirmationModal-message", warningText),
              div(
                cls := "ConfirmationModal-buttons",
                button(
                  cls := "open-spaces-button",
                  cls := "ConfirmationModal-button ConfirmationModal-button--cancel",
                  "Cancel",
                  onClick --> Observer(_ => cancelCreate()),
                ),
                button(
                  cls := "open-spaces-button",
                  cls := "ConfirmationModal-button ConfirmationModal-button--confirm",
                  "Leave & Create",
                  onClick --> Observer(_ => confirmCreate()),
                ),
              ),
            ),
          )
        case None =>
          div()
      },
      
      // My current project section
      child <-- $myProject.map {
        case Some(project) =>
          div(
            cls := "HackathonProjects-myProject",
            h3(cls := "HackathonProjects-sectionTitle", "Your Project"),
            HackathonProjectCard(
              project = project,
              currentUser = name.now(),
              isMyProject = true,
              onLeave = Some(() => handleLeaveProject(project)),
              onJoin = None,
              sendHackathonAction = sendHackathonAction,
              connectionStatus = connectionStatus,
            ),
          )
        case None =>
          div(
            cls := "HackathonProjects-noProject",
            h3(cls := "HackathonProjects-sectionTitle", "You haven't joined a project yet"),
            p(cls := "HackathonProjects-subtitle", "Create your own or join one below!"),
          )
      },
      
      // Create project section
      div(
        cls := "HackathonProjects-create",
        child <-- showCreateForm.signal.map {
          case false =>
            button(
              cls := "open-spaces-button",
              cls := "HackathonProjects-createButton",
              "✨ Propose a Project",
              onClick --> Observer(_ => showCreateForm.set(true)),
            )
          case true =>
            div(
              cls := "HackathonProjects-createForm",
              input(
                cls := "HackathonProjects-input",
                typ := "text",
                placeholder := "What do you want to build?",
                controlled(
                  value <-- newProjectTitle.signal,
                  onInput.mapToValue --> newProjectTitle.writer,
                ),
                onKeyDown --> Observer { (e: dom.KeyboardEvent) =>
                  if e.key == "Enter" then handleCreateProject()
                  else if e.key == "Escape" then
                    showCreateForm.set(false)
                    newProjectTitle.set("")
                },
                onMountFocus,
              ),
              div(
                cls := "HackathonProjects-createFormButtons",
                button(
                  cls := "open-spaces-button",
                  cls := "HackathonProjects-submitButton",
                  "Create",
                  onClick --> Observer(_ => handleCreateProject()),
                ),
                button(
                  cls := "open-spaces-button",
                  cls := "HackathonProjects-cancelButton",
                  "Cancel",
                  onClick --> Observer { _ =>
                    showCreateForm.set(false)
                    newProjectTitle.set("")
                  },
                ),
              ),
            )
        },
      ),
      
      // Other projects section
      div(
        cls := "HackathonProjects-list",
        h3(cls := "HackathonProjects-sectionTitle", "All Projects"),
        child <-- $otherProjects.map { projects =>
          if projects.isEmpty then
            div(cls := "HackathonProjects-empty", "No projects yet. Be the first to propose one!")
          else
            div(
              projects.map { project =>
                HackathonProjectCard(
                  project = project,
                  currentUser = name.now(),
                  isMyProject = false,
                  onLeave = None,
                  onJoin = Some(() => handleJoinProject(project)),
                  sendHackathonAction = sendHackathonAction,
                  connectionStatus = connectionStatus,
                )
              },
            )
        },
      ),
    )

/** Card component for displaying a hackathon project */
object HackathonProjectCard:
  def apply(
    project: HackathonProject,
    currentUser: Person,
    isMyProject: Boolean,
    onLeave: Option[() => Unit],
    onJoin: Option[() => Unit],
    sendHackathonAction: HackathonProjectAction => Unit,
    connectionStatus: ConnectionStatusUI,
  ): HtmlElement =
    val isOwner = project.isOwner(currentUser)
    val memberCount = project.memberCount
    val isLarge = project.isLargeGroup
    
    div(
      cls := "HackathonProjectCard",
      cls := (if isMyProject then "HackathonProjectCard--mine" else ""),
      cls := (if isLarge then "HackathonProjectCard--large" else ""),
      
      // Project title
      div(
        cls := "HackathonProjectCard-header",
        h4(cls := "HackathonProjectCard-title", project.titleText),
      ),
      
      // Member info
      div(
        cls := "HackathonProjectCard-members",
        // Member avatars (first 5)
        div(
          cls := "HackathonProjectCard-avatars",
          project.members.take(5).map { member =>
            img(
              cls := "HackathonProjectCard-avatar",
              src := s"https://github.com/${member.person.unwrap}.png?size=40",
              alt := member.person.unwrap,
              title := member.person.unwrap,
            )
          },
          if memberCount > 5 then
            span(cls := "HackathonProjectCard-moreMembers", s"+${memberCount - 5}")
          else
            span(),
        ),
        span(
          cls := "HackathonProjectCard-memberCount",
          if memberCount == 1 then "1 person"
          else s"$memberCount people",
        ),
      ),
      
      // Large group warning
      if isLarge && !isMyProject then
        div(
          cls := "HackathonProjectCard-largeWarning",
          "⚠️ This group is getting big! Consider smaller projects below.",
        )
      else
        span(),
      
      // Slack thread link
      project.slackThreadUrl.map { url =>
        a(
          cls := "HackathonProjectCard-slackLink",
          href := url,
          target := "_blank",
          "💬 Discuss in Slack",
        )
      }.getOrElse(span()),
      
      // Action buttons
      div(
        cls := "HackathonProjectCard-actions",
        onLeave.map { leave =>
          button(
            cls := "open-spaces-button",
            cls := "HackathonProjectCard-leaveButton",
            if isOwner && project.nextOwner.isEmpty then "🗑️ Delete Project"
            else "👋 Leave Project",
            onClick --> Observer(_ => leave()),
          )
        }.getOrElse(span()),
        onJoin.map { join =>
          button(
            cls := "open-spaces-button",
            cls := "HackathonProjectCard-joinButton",
            "🤝 Join Project",
            onClick --> Observer(_ => join()),
          )
        }.getOrElse(span()),
      ),
    )
