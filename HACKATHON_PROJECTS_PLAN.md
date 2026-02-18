# Hackathon Projects Feature Plan

**Created:** 2026-02-17
**Status:** In Progress

## Overview

Wednesday of the conference schedule is hackathon day. People group up to work on projects brainstormed throughout the week. This feature allows attendees to propose projects, join existing ones, and seamlessly transition between projects.

## Key Requirements

1. **One project per person** — seamless transitions allowed at any time
2. **Fluid throughout the day** — no phases, continuous experience
3. **Title only required** — description/discussion happens in Slack thread
4. **Auto-ownership transfer** — when owner leaves, earliest joiner becomes owner
5. **Confirmation UX** — clear warning when switching projects (shows who inherits)
6. **Large group nudge** — suggest other projects when any project exceeds 5 members
7. **Separate Slack channel** — `hackday-projects` (or `-test` for development)
8. **Separate tab/view** — not integrated into schedule grid

---

## Data Model

### Shared Types (`shared_code/.../HackathonProject.scala`)

```scala
type HackathonProjectId = HackathonProjectId.Type
object HackathonProjectId extends Newtype[Long]

type ProjectTitle = ProjectTitle.Type  
object ProjectTitle extends Newtype[String]:
  override inline def validate(value: String) =
    if value.trim.length < 3 then "Title must be at least 3 characters"
    else if value.trim.length > 80 then "Title must be under 80 characters"
    else true

case class ProjectMember(
  person: Person,
  joinedAtEpochMs: Long,
) derives JsonCodec

case class HackathonProject(
  id: HackathonProjectId,
  title: ProjectTitle,
  owner: Person,
  members: List[ProjectMember],  // Ordered by join time
  createdAtEpochMs: Long,
  slackThreadUrl: Option[String],
) derives JsonCodec:
  def memberCount: Int = members.size
  def isLargeGroup: Boolean = memberCount > 5
  def nextOwner: Option[Person] = 
    members.filterNot(_.person == owner).sortBy(_.joinedAtEpochMs).headOption.map(_.person)
```

### Actions (`shared_code/.../HackathonProjectAction.scala`)

```scala
enum HackathonProjectAction derives JsonCodec:
  case Create(title: ProjectTitle, owner: Person)
  case Join(projectId: HackathonProjectId, person: Person)
  case Leave(projectId: HackathonProjectId, person: Person)
  case Rename(projectId: HackathonProjectId, newTitle: ProjectTitle)
  case Delete(projectId: HackathonProjectId, requester: Person)

enum HackathonProjectActionConfirmed derives JsonCodec:
  case Created(project: HackathonProject)
  case Joined(projectId: HackathonProjectId, person: Person, joinedAtEpochMs: Long)
  case Left(projectId: HackathonProjectId, person: Person, newOwner: Option[Person])
  case OwnershipTransferred(projectId: HackathonProjectId, newOwner: Person)
  case Renamed(projectId: HackathonProjectId, newTitle: ProjectTitle)
  case Deleted(projectId: HackathonProjectId)
  case SlackThreadLinked(projectId: HackathonProjectId, slackThreadUrl: String)
  case StateReplace(projects: List[HackathonProject])
  case Rejected(action: HackathonProjectAction)
```

### State (`shared_code/.../HackathonProjectState.scala`)

```scala
case class HackathonProjectState(
  projects: Map[HackathonProjectId, HackathonProject],
):
  def personCurrentProject(person: Person): Option[HackathonProject] =
    projects.values.find(_.members.exists(_.person == person))
    
  def projectsExcludingPerson(person: Person): List[HackathonProject] =
    projects.values.filterNot(_.members.exists(_.person == person)).toList
    
  def projectsSortedBySize: List[HackathonProject] =
    projects.values.toList.sortBy(p => (-p.memberCount, p.createdAtEpochMs))
    
  def smallerProjects(excludeId: HackathonProjectId): List[HackathonProject] =
    projects.values.filter(p => p.id != excludeId && p.memberCount < 5).toList
```

### WebSocket Messages (add to `DiscussionAction.scala`)

```scala
case class HackathonProjectActionMessage(
  action: HackathonProjectAction,
) extends WebSocketMessageFromClient derives JsonCodec

case class HackathonProjectActionConfirmedMessage(
  event: HackathonProjectActionConfirmed,
) extends WebSocketMessageFromServer derives JsonCodec
```

---

## Persistence

### Database Tables

```sql
CREATE TABLE hackathon_projects (
  id BIGINT PRIMARY KEY,
  title VARCHAR(80) NOT NULL,
  owner VARCHAR(255) NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL,
  deleted_at TIMESTAMP WITH TIME ZONE,
  slack_channel_id VARCHAR(50),
  slack_thread_ts VARCHAR(50),
  slack_permalink TEXT
);

CREATE TABLE hackathon_project_members (
  project_id BIGINT NOT NULL REFERENCES hackathon_projects(id),
  github_username VARCHAR(255) NOT NULL,
  joined_at TIMESTAMP WITH TIME ZONE NOT NULL,
  left_at TIMESTAMP WITH TIME ZONE,
  PRIMARY KEY (project_id, github_username)
);
```

### Entities (`server/.../db/Entities.scala`)

```scala
case class HackathonProjectRow(
  id: Long,
  title: String,
  owner: String,
  createdAt: OffsetDateTime,
  deletedAt: Option[OffsetDateTime],
  slackChannelId: Option[String],
  slackThreadTs: Option[String],
  slackPermalink: Option[String],
) derives DbCodec

case class HackathonProjectMemberRow(
  projectId: Long,
  githubUsername: String,
  joinedAt: OffsetDateTime,
  leftAt: Option[OffsetDateTime],
) derives DbCodec
```

### Repositories (`server/.../db/Repositories.scala`)

```scala
class HackathonProjectRepository(using DbCon):
  def insert(row: HackathonProjectRow): Task[Unit]
  def update(row: HackathonProjectRow): Task[Unit]
  def softDelete(id: Long): Task[Unit]
  def findById(id: Long): Task[Option[HackathonProjectRow]]
  def findAllActive: Task[List[HackathonProjectRow]]
  def updateOwner(id: Long, newOwner: String): Task[Unit]
  def updateSlackThread(id: Long, channelId: String, ts: String, permalink: String): Task[Unit]

class HackathonProjectMemberRepository(using DbCon):
  def addMember(projectId: Long, username: String): Task[Unit]
  def removeMember(projectId: Long, username: String): Task[Unit]
  def findActiveByProject(projectId: Long): Task[List[HackathonProjectMemberRow]]
  def findActiveByUser(username: String): Task[Option[HackathonProjectMemberRow]]
```

---

## Server Components

### Service (`server/.../HackathonProjectService.scala`)

Core logic:
- `applyAction` handles all actions with proper validation
- `Join` automatically handles leaving current project first
- Ownership transfer happens atomically when owner leaves

### Slack Integration (`server/.../slack/SlackNotifier.scala`)

Add `notifyHackathonProject` method:
- **Create:** Post thread "🛠️ *{title}* — Proposed by *{owner}*"
- **Join:** Reply "👋 *{person}* joined the project"
- **Leave:** Reply "👋 *{person}* left" + "🔑 *{newOwner}* is now leading" if transferred
- **Delete:** Delete the thread message

Channel: `hackday-projects` (configurable, separate from discussion topics)

### WebSocket Handler (`server/.../BackendSocketApp.scala`)

Add case for `HackathonProjectActionMessage` in message handler.

---

## Client Components

### State (`client/.../AppState.scala`)

Add:
```scala
hackathonProjectState: Var[HackathonProjectState]
sendHackathonAction: HackathonProjectAction => Unit
```

### View (`client/.../components/HackathonProjectsView.scala`)

**Layout:**
1. **Top section:** Current project (if any)
   - Project title (editable if owner)
   - Member avatars + count
   - "Leave Project" button
   
2. **Create section:** "Propose a Project" button → title input

3. **Browse section:** All other projects
   - Cards with swipe-to-join
   - Large group indicator (>5 members)
   - "Consider these smaller projects" nudge

### Confirmation Modal

When user tries to join a new project while already in one:
- If owner with members: "You'll hand ownership to {nextPerson}. Continue?"
- If owner alone: "This project will be deleted. Continue?"
- If member: "You'll leave {currentProject}. Continue?"

### Tab Integration (`client/.../FrontEnd.scala`)

Add "Hackathon" tab alongside existing tabs.

---

## Implementation Order

### Phase 1: Shared Models ✅
- [x] `HackathonProject.scala` - types and model
- [x] `HackathonProjectAction.scala` - actions (added to DiscussionAction.scala)
- [x] `HackathonProjectState.scala` - state management
- [x] Update `DiscussionAction.scala` with WS message types

### Phase 2: Server Persistence ✅
- [x] Add entities to `Entities.scala`
- [x] Add repositories to `Repositories.scala`
- [x] Create `HackathonProjectService.scala`
- [ ] Add to dependency wiring (Backend.scala) — do in Phase 3

### Phase 3: Server Integration ✅
- [x] Update `SlackNotifier.scala` with hackathon methods
- [x] Update `SlackConfig` for separate channel
- [x] Update `DiscussionService.scala` to handle messages (this handles WS messages, not BackendSocketApp)
- [x] Update `Backend.scala` to wire in new services
- [x] Database migration script (V8__hackathon_projects.sql)

### Phase 4: Client UI ✅
- [x] Update `AppState.scala`
- [x] Create `HackathonProjectsView.scala` (includes HackathonProjectCard)
- [x] Add confirmation modal component (in HackathonProjectsView)
- [x] Update `FrontEnd.scala` with tab
- [x] Update `ViewToggle.scala` with Hackathon button
- [x] Add `AppView.Hackathon` case

### Phase 5: Polish
- [x] Large group nudge UI (warning on cards with >5 members)
- [ ] Swipe animations (deferred - using buttons for MVP)
- [x] Error handling
- [ ] CSS styling (needs styles for new components)
- [ ] Testing

---

## File Checklist

| File | Status | Notes |
|------|--------|-------|
| `shared_code/.../HackathonProject.scala` | ⬜ | New file |
| `shared_code/.../HackathonProjectState.scala` | ⬜ | New file |
| `shared_code/.../DiscussionAction.scala` | ⬜ | Add WS messages |
| `server/.../db/Entities.scala` | ⬜ | Add row types |
| `server/.../db/Repositories.scala` | ⬜ | Add repositories |
| `server/.../HackathonProjectService.scala` | ⬜ | New file |
| `server/.../slack/SlackNotifier.scala` | ⬜ | Add methods |
| `server/.../slack/SlackConfig.scala` | ⬜ | Add hackathon channel |
| `server/.../BackendSocketApp.scala` | ⬜ | Handle new messages |
| `server/.../Backend.scala` | ⬜ | Wire dependencies |
| `client/.../AppState.scala` | ⬜ | Add state |
| `client/.../components/HackathonProjectsView.scala` | ⬜ | New file |
| `client/.../components/HackathonProjectCard.scala` | ⬜ | New file |
| `client/.../components/ConfirmationModal.scala` | ⬜ | New file (reusable) |
| `client/.../FrontEnd.scala` | ⬜ | Add tab |

---

## Notes

- Slack bot already has channel creation capability (see `SlackClient.createChannel`)
- Reusing `Person` type from existing code
- Reusing `SwipeableCard` pattern but needs confirmation step
- No glyphicons for now (can add later)
- No location tracking for now (can add GPS/address later)
