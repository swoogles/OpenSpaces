# Activities Feature Plan

## Goals
- Add a new backend entity: `Activity` (distinct from `LightningTalk`).
- Users can create an Activity with:
  - short text description
  - event time
- Event time must be near conference timeslots (not far-future like June 2030).
- Users can express interest, similar to hackathon join / discussion voting.
- Activities should use swipeable cards in mobile UI.
- UI tab currently named `Lightning` becomes `Activities`:
  - existing Lightning controls remain at the top
  - new generic Activities section appears below
- Activities should eventually appear in schedule view, ordered properly.
- Explore a common trait for cross-entity schedule ordering/rendering.

## Confirmed Decisions (From You)
- Time validation: activity event time must be within `+- 3 days` of conference timeslots.
- Ownership/editing: only activity owner can edit.
- Interest model: binary interest (aligned with current topic vote / hackathon join intent).
- Slack integration: yes, create and maintain Slack threads for activities.
- Replay + chaos: include activities in replay and random chaos features.
- Schedule rendering: activities are `timeline rows`, not room-bound; there is no room concept for activities.
- Schedule visibility: show all activities (at least for now).

## Detailed Questions For You

### Product and behavior
1. Should an `Activity` be single-owner (creator) plus interested users, or should anyone be able to edit/delete it?
2. Do we want `interested` as binary (interested/not interested), or weighted reactions (like discussions)?
3. Should users be limited to one activity at a given time window, similar to being in one hackathon project?
4. Should activities support a max attendee cap, or unlimited interest?
5. Should creator auto-count as interested on creation?
6. Should activities be deletable by creator only, admins only, or both?
7. Should activities be renameable/editable (description/time) after creation?

### Time validation rules
8. What is “within a few days” exactly: `±2`, `±3`, `±5`, or another number?
9. Should the valid window be derived dynamically from min/max DB `time_slots.start_time` (recommended), or fixed dates?
10. Should exact conference week boundaries include evenings (for after-hours activities), or only daytime slot range?
11. Should event time be any timestamp, or snapped to known slot boundaries (start times)?
12. Should timezone always be server-local, UTC, or conference-local timezone?
13. If conference slots are not loaded yet, should activity creation be blocked or allowed with relaxed validation?

### Schedule integration (remaining)
14. Should Lightning talks and Activities both appear in schedule, or Activities only for now?

### UI/UX details (mobile-first)
19. For Activity cards, should swipe left = not interested and right = interested (same as discussions), or join/leave style like hackathon?
20. Should Activities list default sort be:
    - soonest event time first (recommended), or
    - most interest first?
21. Should there be separate sections: `My Activities`, `Suggested`, `All`?
22. Should create form require date+time picker only, or quick presets (Today afternoon, Tonight, etc.)?
23. Should we display relative time labels (“Today 2:30 PM”) plus exact timestamp?

### Data and integrations
24. Do Activities need Slack thread creation like discussions/hackathon/lightning?
25. Should random chaos mode generate random Activities too?
26. Should replay/confirmed-actions include Activities events?
27. Should we support hard-delete, soft-delete, or match hackathon/discussions patterns?

## Proposed Architecture

### Shared model (shared_code)
- Add package `co.wtf.openspaces.activities` with:
  - `ActivityId` newtype
  - `ActivityDescription` newtype (length bounds, trimmed)
  - `Activity` case class (`id`, `description`, `creator`, `interestedParties`, `eventTime`, `createdAtEpochMs`, optional display/slack fields)
  - `ActivityAction` / `ActivityActionConfirmed`
  - `ActivityState`

### Server
- Add DB migration(s):
  - `activities` table
  - `activity_interest` table (or equivalent)
  - optional slack thread columns if needed
- Add `ActivityRow` (and interest row) in `Entities.scala`.
- Add `ActivityRepository` in `Repositories.scala`.
- Add `ActivityService` parallel to `LightningTalkService`/`HackathonProjectService`.
- Add websocket message types in `WebSocketMessage.scala`:
  - `ActivityActionMessage`
  - `ActivityActionConfirmedMessage`
- Extend `SessionService`:
  - initial `StateReplace` for activities on ticket auth
  - ticketed/unticketed action handlers
  - broadcast + `confirmed_actions` persistence + actor extraction
- Optional: Slack notifier hooks if desired.

### Time-window validation
- Server-side source of truth:
  - query `time_slots` min/max timestamps
  - enforce allowed range using configured day buffer (`3` days)
- Mirror validation on client for quick UX feedback, but rely on server for correctness.

### Frontend (Laminar / Scala.js)
- Add `AppState.activityState: Var[ActivityState]`.
- Add websocket handling in `FrontEnd.scala`.
- Rename tab label `Lightning` -> `Activities` in `ViewToggle`.
- Replace `LightningTalksView` usage with new composite `ActivitiesView`:
  - top section: existing Lightning controls unchanged
  - bottom section: Activity create + swipeable cards + interest actions
- Use existing `SwipeableCard` for Activity cards, typed action enum.
- Keep strict typing; no dynamic JS.

### Schedule rendering + common trait
- Add a shared schedule abstraction (name tentative):
  - `trait SchedulableItem` with fields/methods for:
    - `startTime: java.time.LocalDateTime`
    - `sortKey`
    - `renderKind` (discussion/activity/lightning/etc)
    - stable id
- Implement for:
  - `Discussion` (when scheduled)
  - `Activity`
  - optionally `Lightning` slot projections
- Update `LinearScheduleView` to merge/sort schedulable items per day/timeline.

## Incremental Delivery Plan

1. **Foundation + decisions**
   - finalize open questions above
   - define activity data model and validation rules

2. **Backend activity core**
   - migration + repository + service
   - websocket messages + session integration
   - state replace + broadcast flow
   - tests for action/state transitions and auth behavior

3. **Frontend activities tab**
   - add `Activities` tab label
   - create composite view (Lightning + Activities)
   - implement create form + client validation
   - implement swipe interest actions and empty/loading states

4. **Schedule integration**
   - implement `SchedulableItem` abstraction
   - merge activities into linear schedule with deterministic ordering
   - render in-between slot activities when timestamps fall between discussion slots

5. **Polish + reliability**
   - confirm websocket multi-client consistency
   - add/revise tests (shared + server + UI behavior where possible)
   - optional Slack/replay/random-action integration

## Risks / Notes
- Race conditions around interest toggles and edits should follow existing “confirmed action” pattern.
- Schedule integration is easiest in `LinearScheduleView` first; grid view can follow later.
- The trait refactor should be introduced incrementally to avoid touching all entities at once.

## Finalized Implementation Decisions
- Activities render in `LinearScheduleView` as timeline rows (no room binding).
- Activities that occur between conference slots render between those slots in chronological order.
- Same-timestamp activity tie-break: higher interest count first, then earlier `createdAtEpochMs`, then id.
- Schedule shows all activities (no minimum interest threshold for now).
- Schedule continues to show both Lightning talks and Activities.
- Owner-only permissions for edits/deletes are enforced server-side in `ActivityService`; client mirrors for UX only.
- Time validation is enforced server-side with conference bounds:
  - `allowedStart = min(time_slots.start_time) - 3 days`
  - `allowedEnd = max(time_slots.start_time) + 3 days`
- Replay and random chaos include Activity confirmed actions.
- Slack thread lifecycle matches existing entity patterns:
  - create thread on activity creation
  - update thread on activity edit/time changes
  - delete thread on activity delete

## Build Order (Execution-Ready)
1. Add shared `activities` domain types, actions, state, and codecs.
2. Add DB migration + repositories for activities and activity interests.
3. Add `ActivityService` with validation, ownership checks, and binary interest actions.
4. Extend websocket protocol + `SessionService` (state replace, action routing, broadcast, confirmed-action logging).
5. Extend `SlackNotifier` for activity create/update/delete + thread-link confirmed action.
6. Add frontend `activityState`, websocket handling, and typed send action function.
7. Rename `Lightning` tab label to `Activities` and introduce composite `ActivitiesView`:
   - existing lightning controls at top
   - activity create + browse + swipe cards below
8. Extend `LinearScheduleView` with schedulable merge and activity timeline rows.
9. Introduce common `SchedulableItem` trait and migrate rendering path incrementally.
10. Add tests:
   - shared state/action tests
   - server service + auth + time validation tests
   - websocket integration tests for multi-client consistency and state replace
   - replay/random action coverage for activities
