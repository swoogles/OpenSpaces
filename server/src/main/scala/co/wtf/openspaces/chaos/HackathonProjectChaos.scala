package co.wtf.openspaces.chaos

import co.wtf.openspaces.{Person, RandomUsers}
import co.wtf.openspaces.hackathon.*
import zio.*

/** Random hackathon actions for chaos mode. */
object HackathonProjectChaos:

  /** Weighted distribution:
    * - Person with no project: 70% join an existing project, otherwise create one
    * - Person with a project: 35% switch project, 20% leave, 5% delete own (owner only),
    *   otherwise attempt a create (rejected, since they already have a project)
    *
    * None when the chosen action has no valid target.
    */
  def randomAction(current: HackathonProjectState): UIO[Option[HackathonProjectAction]] =
    for
      person <- RandomUsers.randomPerson
      roll   <- Random.nextIntBounded(100)
      action <- (current.personCurrentProject(person), roll) match
        case (None, r) if r < 70 && current.projects.nonEmpty =>
          randomProject(current.projects.values.toList).flatMap {
            case Some(project) => ZIO.some(HackathonProjectAction.Join(project.id, person))
            case None          => createAction(person).asSome
          }
        case (None, _) =>
          createAction(person).asSome
        case (Some(myProject), r) if r < 35 && current.projects.size > 1 =>
          randomProject(current.projects.values.filterNot(_.id == myProject.id).toList)
            .map(_.map(other => HackathonProjectAction.Join(other.id, person)))
        case (Some(myProject), r) if r < 55 =>
          ZIO.some(HackathonProjectAction.Leave(myProject.id, person))
        case (Some(myProject), r) if r < 60 && myProject.isOwner(person) =>
          ZIO.some(HackathonProjectAction.Delete(myProject.id, person))
        case _ =>
          createAction(person).asSome
    yield action

  private def createAction(person: Person): UIO[HackathonProjectAction] =
    Random
      .nextIntBounded(randomProjectTitles.size)
      .map(i => HackathonProjectAction.Create(randomProjectTitles(i), person))

  private def randomProject(projects: List[HackathonProject]): UIO[Option[HackathonProject]] =
    if projects.isEmpty then ZIO.none
    else Random.nextIntBounded(projects.size).map(i => Some(projects(i)))

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
