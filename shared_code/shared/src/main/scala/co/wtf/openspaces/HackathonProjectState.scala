package co.wtf.openspaces

import neotype.unwrap

case class HackathonProjectState(
  projects: Map[HackathonProjectId, HackathonProject],
):

  /** Find the project a person is currently involved in (if any) */
  def personCurrentProject(person: Person): Option[HackathonProject] =
    projects.values.find(_.hasMember(person))

  /** Check if a person is currently in any project */
  def personHasProject(person: Person): Boolean =
    personCurrentProject(person).isDefined

  /** Get all projects excluding ones the person is in */
  def projectsExcludingPerson(person: Person): List[HackathonProject] =
    projects.values.filterNot(_.hasMember(person)).toList

  /** All projects sorted by member count (descending), then creation time */
  def projectsSortedBySize: List[HackathonProject] =
    projects.values.toList.sortBy(p => (-p.memberCount, p.createdAtEpochMs))

  /** Projects with fewer than 5 members, excluding the given project */
  def smallerProjects(excludeId: HackathonProjectId): List[HackathonProject] =
    projects.values
      .filter(p => p.id != excludeId && !p.isLargeGroup)
      .toList
      .sortBy(p => (-p.memberCount, p.createdAtEpochMs))

  /** All projects sorted for display (user's project first if any, then by size) */
  def projectsForDisplay(person: Person): (Option[HackathonProject], List[HackathonProject]) =
    val myProject = personCurrentProject(person)
    val otherProjects = projectsExcludingPerson(person).sortBy(p => (-p.memberCount, p.createdAtEpochMs))
    (myProject, otherProjects)

  def apply(action: HackathonProjectActionConfirmed): HackathonProjectState =
    action match
      case HackathonProjectActionConfirmed.StateReplace(newProjects) =>
        copy(projects = newProjects.map(p => p.id -> p).toMap)

      case HackathonProjectActionConfirmed.Created(project) =>
        copy(projects = projects + (project.id -> project))

      case HackathonProjectActionConfirmed.Joined(projectId, person, joinedAtEpochMs) =>
        copy(projects = projects.updatedWith(projectId) {
          _.map(_.withMember(person, joinedAtEpochMs))
        })

      case HackathonProjectActionConfirmed.Left(projectId, person, newOwner) =>
        copy(projects = projects.updatedWith(projectId) { maybeProject =>
          maybeProject.map { project =>
            val withoutMember = project.withoutMember(person)
            newOwner.fold(withoutMember)(withoutMember.withOwner)
          }
        })

      case HackathonProjectActionConfirmed.OwnershipTransferred(projectId, newOwner) =>
        copy(projects = projects.updatedWith(projectId) {
          _.map(_.withOwner(newOwner))
        })

      case HackathonProjectActionConfirmed.Renamed(projectId, newTitle) =>
        copy(projects = projects.updatedWith(projectId) {
          _.map(_.copy(title = newTitle))
        })

      case HackathonProjectActionConfirmed.Deleted(projectId) =>
        copy(projects = projects - projectId)

      case HackathonProjectActionConfirmed.SlackThreadLinked(projectId, slackThreadUrl) =>
        copy(projects = projects.updatedWith(projectId) {
          _.map(_.copy(slackThreadUrl = Some(slackThreadUrl)))
        })

      case HackathonProjectActionConfirmed.Rejected(_) =>
        this

      case HackathonProjectActionConfirmed.Unauthorized(_) =>
        this

object HackathonProjectState:
  val empty: HackathonProjectState = HackathonProjectState(Map.empty)
