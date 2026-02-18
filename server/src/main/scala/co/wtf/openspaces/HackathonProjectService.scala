package co.wtf.openspaces

import co.wtf.openspaces.db.{
  HackathonProjectRepository,
  HackathonProjectMemberRepository,
  HackathonProjectRow,
  HackathonProjectMemberRow,
  UserRepository,
}
import neotype.unwrap
import zio.*

case class HackathonProjectService(
  state: Ref[HackathonProjectState],
  projectRepo: HackathonProjectRepository,
  memberRepo: HackathonProjectMemberRepository,
  userRepo: UserRepository,
  gitHubProfileService: GitHubProfileService,
):

  def snapshot: UIO[HackathonProjectState] =
    state.get

  def applyConfirmed(action: HackathonProjectActionConfirmed): UIO[Unit] =
    state.update(_(action))

  def listProjects: UIO[List[HackathonProject]] =
    snapshot.map(_.projectsSortedBySize)

  def applyAction(
    action: HackathonProjectAction,
  ): Task[List[HackathonProjectActionConfirmed]] =
    action match
      case create @ HackathonProjectAction.Create(title, creator) =>
        for
          _ <- ensureUserExists(creator.unwrap)
          current <- state.get
          result <- current.personCurrentProject(creator) match
            // User already has a project - reject
            case Some(_) =>
              ZIO.succeed(List(HackathonProjectActionConfirmed.Rejected(create)))
            case None =>
              for
                project <- createProject(title, creator)
                _ <- persistProject(project)
                _ <- state.update(_(HackathonProjectActionConfirmed.Created(project)))
              yield List(HackathonProjectActionConfirmed.Created(project))
        yield result

      case join @ HackathonProjectAction.Join(projectId, person) =>
        for
          _ <- ensureUserExists(person.unwrap)
          current <- state.get
          targetProject = current.projects.get(projectId)
          currentProject = current.personCurrentProject(person)
          result <- (targetProject, currentProject) match
            // Target doesn't exist
            case (None, _) =>
              ZIO.succeed(List(HackathonProjectActionConfirmed.Rejected(join)))
            // Already in target project
            case (Some(target), Some(curr)) if target.id == curr.id =>
              ZIO.succeed(List(HackathonProjectActionConfirmed.Rejected(join)))
            // Need to leave current project first
            case (Some(target), Some(curr)) =>
              for
                leaveResults <- handleLeave(curr, person)
                joinResult <- handleJoin(target, person)
              yield leaveResults :+ joinResult
            // No current project, just join
            case (Some(target), None) =>
              handleJoin(target, person).map(List(_))
        yield result

      case leave @ HackathonProjectAction.Leave(projectId, person) =>
        for
          current <- state.get
          maybeProject = current.projects.get(projectId)
          result <- maybeProject match
            case None =>
              ZIO.succeed(List(HackathonProjectActionConfirmed.Rejected(leave)))
            case Some(project) if !project.hasMember(person) =>
              ZIO.succeed(List(HackathonProjectActionConfirmed.Rejected(leave)))
            case Some(project) =>
              handleLeave(project, person)
        yield result

      case rename @ HackathonProjectAction.Rename(projectId, newTitle) =>
        for
          current <- state.get
          result <- current.projects.get(projectId) match
            case None =>
              ZIO.succeed(List(HackathonProjectActionConfirmed.Rejected(rename)))
            case Some(project) =>
              for
                _ <- projectRepo.update(toRow(project).copy(title = newTitle.unwrap))
                confirmed = HackathonProjectActionConfirmed.Renamed(projectId, newTitle)
                _ <- state.update(_(confirmed))
              yield List(confirmed)
        yield result

      case delete @ HackathonProjectAction.Delete(projectId, requester) =>
        for
          current <- state.get
          result <- current.projects.get(projectId) match
            case None =>
              ZIO.succeed(List(HackathonProjectActionConfirmed.Rejected(delete)))
            case Some(project) if !project.isOwner(requester) =>
              ZIO.succeed(List(HackathonProjectActionConfirmed.Unauthorized(delete)))
            case Some(project) =>
              for
                _ <- projectRepo.softDelete(projectId.unwrap)
                confirmed = HackathonProjectActionConfirmed.Deleted(projectId)
                _ <- state.update(_(confirmed))
              yield List(confirmed)
        yield result

  private def handleJoin(
    project: HackathonProject,
    person: Person,
  ): Task[HackathonProjectActionConfirmed] =
    for
      memberRow <- memberRepo.addMember(project.id.unwrap, person.unwrap)
      joinedAtEpochMs = memberRow.joinedAt.toInstant.toEpochMilli
      confirmed = HackathonProjectActionConfirmed.Joined(project.id, person, joinedAtEpochMs)
      _ <- state.update(_(confirmed))
    yield confirmed

  private def handleLeave(
    project: HackathonProject,
    person: Person,
  ): Task[List[HackathonProjectActionConfirmed]] =
    val isOwner = project.isOwner(person)
    val nextOwner = project.nextOwner
    val shouldDelete = isOwner && nextOwner.isEmpty

    for
      _ <- memberRepo.removeMember(project.id.unwrap, person.unwrap)
      results <- if shouldDelete then
        // Delete the project
        for
          _ <- projectRepo.softDelete(project.id.unwrap)
          confirmed = HackathonProjectActionConfirmed.Deleted(project.id)
          _ <- state.update(_(confirmed))
        yield List(confirmed)
      else if isOwner then
        // Transfer ownership
        val newOwner = nextOwner.get // Safe because we checked shouldDelete
        for
          _ <- projectRepo.updateOwner(project.id.unwrap, newOwner.unwrap)
          leftConfirmed = HackathonProjectActionConfirmed.Left(project.id, person, Some(newOwner))
          _ <- state.update(_(leftConfirmed))
        yield List(leftConfirmed)
      else
        // Just leave
        val confirmed = HackathonProjectActionConfirmed.Left(project.id, person, None)
        state.update(_(confirmed)).as(List(confirmed))
    yield results

  private def ensureUserExists(username: String): Task[Unit] =
    gitHubProfileService.ensureUserWithDisplayName(username).unit

  private def createProject(
    title: ProjectTitle,
    creator: Person,
  ): Task[HackathonProject] =
    for
      id <- zio.Random.nextLong.map(n => if n == Long.MinValue then 0L else math.abs(n))
      creatorUser <- userRepo.findByUsername(creator.unwrap)
      createdAtEpochMs = java.lang.System.currentTimeMillis()
    yield HackathonProject.create(
      id = HackathonProjectId(id),
      title = title,
      owner = creator,
      ownerDisplayName = creatorUser.flatMap(_.displayName),
      createdAtEpochMs = createdAtEpochMs,
    )

  private def persistProject(project: HackathonProject): Task[Unit] =
    for
      _ <- projectRepo.insert(toRow(project))
      _ <- memberRepo.addMember(project.id.unwrap, project.owner.unwrap)
    yield ()

  /** Generate a random hackathon action for chaos testing.
    * Weighted distribution:
    * - 40% create new project (if person has no project)
    * - 35% join existing project
    * - 20% leave current project
    * - 5% delete own project (owner only)
    */
  def randomHackathonAction: Task[List[HackathonProjectActionConfirmed]] =
    for
      person <- RandomUsers.randomPerson
      current <- state.get
      roll <- zio.Random.nextIntBounded(100)
      result <- (current.personCurrentProject(person), roll) match
        // Person has no project
        case (None, r) if r < 70 && current.projects.nonEmpty =>
          // 70% join existing project
          randomProject(current).flatMap {
            case Some(project) =>
              applyAction(HackathonProjectAction.Join(project.id, person))
            case None =>
              createRandomProject(person)
          }
        case (None, _) =>
          // 30% create new project
          createRandomProject(person)
        
        // Person has a project
        case (Some(myProject), r) if r < 35 && current.projects.size > 1 =>
          // 35% join different project
          randomProjectExcluding(current, myProject.id).flatMap {
            case Some(other) =>
              applyAction(HackathonProjectAction.Join(other.id, person))
            case None =>
              ZIO.succeed(List.empty)
          }
        case (Some(myProject), r) if r < 55 =>
          // 20% leave project
          applyAction(HackathonProjectAction.Leave(myProject.id, person))
        case (Some(myProject), r) if r < 60 && myProject.isOwner(person) =>
          // 5% delete own project (owner only)
          applyAction(HackathonProjectAction.Delete(myProject.id, person))
        case _ =>
          // Otherwise create new project (will be rejected if person already has one)
          createRandomProject(person)
    yield result

  private def createRandomProject(person: Person): Task[List[HackathonProjectActionConfirmed]] =
    for
      titleIdx <- zio.Random.nextIntBounded(randomProjectTitles.size)
      title = randomProjectTitles(titleIdx)
      result <- applyAction(HackathonProjectAction.Create(title, person))
    yield result

  private def randomProject(state: HackathonProjectState): UIO[Option[HackathonProject]] =
    val projects = state.projects.values.toList
    if projects.isEmpty then ZIO.none
    else zio.Random.nextIntBounded(projects.size).map(i => Some(projects(i)))

  private def randomProjectExcluding(state: HackathonProjectState, excludeId: HackathonProjectId): UIO[Option[HackathonProject]] =
    val projects = state.projects.values.filterNot(_.id == excludeId).toList
    if projects.isEmpty then ZIO.none
    else zio.Random.nextIntBounded(projects.size).map(i => Some(projects(i)))

  private val randomProjectTitles: List[ProjectTitle] = List(
    "AI-powered coffee maker",
    "Distributed cat herding",
    "Blockchain for breakfast",
    "Serverless toaster",
    "Machine learning linter",
    "Kubernetes for houseplants",
    "GraphQL pizza ordering",
    "WebAssembly sudoku solver",
    "Rust rewrite of everything",
    "TypeScript type tetris",
    "Docker compose symphony",
    "Microservices for ants",
    "Event sourcing diary",
    "CQRS recipe book",
    "Functional programming game",
  ).flatMap(ProjectTitle.make(_).toOption)

  private def toRow(project: HackathonProject): HackathonProjectRow =
    val createdAt = java.time.OffsetDateTime.ofInstant(
      java.time.Instant.ofEpochMilli(project.createdAtEpochMs),
      java.time.ZoneOffset.UTC,
    )
    HackathonProjectRow(
      id = project.id.unwrap,
      title = project.title.unwrap,
      owner = project.owner.unwrap,
      createdAt = createdAt,
      deletedAt = None,
      slackChannelId = None,
      slackThreadTs = None,
      slackPermalink = project.slackThreadUrl,
    )

object HackathonProjectService:
  private def fromRows(
    projectRow: HackathonProjectRow,
    memberRows: Vector[HackathonProjectMemberRow],
    displayNames: Map[String, Option[String]],
  ): HackathonProject =
    val members = memberRows.map { row =>
      ProjectMember(
        person = Person(row.githubUsername),
        joinedAtEpochMs = row.joinedAt.toInstant.toEpochMilli,
      )
    }.toList.sorted
    
    HackathonProject(
      id = HackathonProjectId(projectRow.id),
      title = ProjectTitle.make(projectRow.title).getOrElse(ProjectTitle.unsafeMake(projectRow.title)),
      owner = Person(projectRow.owner),
      members = members,
      createdAtEpochMs = projectRow.createdAt.toInstant.toEpochMilli,
      ownerDisplayName = displayNames.get(projectRow.owner).flatten,
      slackThreadUrl = projectRow.slackPermalink,
    )

  def loadInitialState(
    projectRepo: HackathonProjectRepository,
    memberRepo: HackathonProjectMemberRepository,
    gitHubProfileService: GitHubProfileService,
  ): Task[HackathonProjectState] =
    for
      projectRows <- projectRepo.findAllActive
      projects <- ZIO.foreach(projectRows) { projectRow =>
        for
          memberRows <- memberRepo.findActiveByProject(projectRow.id)
          usernames = (projectRow.owner :: memberRows.map(_.githubUsername).toList).distinct
          userRows <- ZIO.foreach(usernames)(username =>
            gitHubProfileService.ensureUserWithDisplayName(username).either.map(_.toOption),
          )
          displayNames = userRows.flatten.map(u => u.githubUsername -> u.displayName).toMap
        yield fromRows(projectRow, memberRows, displayNames)
      }
    yield HackathonProjectState(projects.map(p => p.id -> p).toMap)

  val layer: ZLayer[
    HackathonProjectRepository & HackathonProjectMemberRepository & UserRepository & GitHubProfileService,
    Throwable,
    HackathonProjectService,
  ] =
    ZLayer.fromZIO:
      for
        projectRepo <- ZIO.service[HackathonProjectRepository]
        memberRepo <- ZIO.service[HackathonProjectMemberRepository]
        userRepo <- ZIO.service[UserRepository]
        gitHubProfileService <- ZIO.service[GitHubProfileService]
        initialState <- loadInitialState(projectRepo, memberRepo, gitHubProfileService)
        stateRef <- Ref.make(initialState)
      yield HackathonProjectService(
        stateRef,
        projectRepo,
        memberRepo,
        userRepo,
        gitHubProfileService,
      )
