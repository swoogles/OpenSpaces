# Refactoring Plan: Code Organization

**Goal**: Break up large files into discoverable, file-browser-friendly modules.

**Status**: 🔄 In Progress

---

## Phase 1: Break up FrontEnd.scala (~2900 lines → ~15 files)

Current `FrontEnd.scala` contains:
- 5 utility objects (SlotPositionTracker, SwapAnimationState, MenuPositioning, ScrollPreserver, ToastManager)
- Main app object with state
- ~15 component functions
- Helper functions

### Step 1.1: Extract utility objects ✅ DONE (commit e499dba)
Extract these standalone objects (no dependencies on FrontEnd state):

| File | Object | Lines | Status |
|------|--------|-------|--------|
| `util/SlotPositionTracker.scala` | SlotPositionTracker | ~90 | ✅ |
| `util/SwapAnimationState.scala` | SwapAnimationState | ~170 | ✅ |
| `util/MenuPositioning.scala` | MenuPositioning | ~25 | ✅ |
| `util/ScrollPreserver.scala` | ScrollPreserver | ~25 | ✅ |
| `components/Toast.scala` | ToastManager | ~45 | ✅ |

### Step 1.2: Extract components 🔄 IN PROGRESS
Components that can be standalone functions/objects:

| File | Component | Lines | Status |
|------|-----------|-------|--------|
| `components/AdminControls.scala` | AdminControls + helpers | ~210 | ✅ |
| `components/TopicSubmission.scala` | TopicSubmission | ~50 | ✅ |
| `components/SwipeableCard.scala` | SwipeableCard + SwipeState | ~175 | ✅ |
| `components/TopicCard.scala` | SingleDiscussionComponent | ~140 | ❌ |
| `components/ScheduleView.scala` | ScheduleView, LinearScheduleView, SlotSchedules | ~200 | ❌ |
| `components/ScheduleSlot.scala` | ScheduleSlotComponent, SlotSchedule | ~100 | ❌ |
| `components/VoteButtons.scala` | VoteButtons | ~70 | ✅ |
| `components/ViewToggle.scala` | ViewToggle | ~35 | ✅ |
| `components/Menus.scala` | Menu, UnscheduledDiscussionsMenu, ActiveDiscussionActionMenu | ~325 | ✅ |
| `components/InlineEditableTitle.scala` | InlineEditableTitle | ~110 | ✅ |
| `components/ErrorBanner.scala` | ErrorBanner | ~45 | ✅ |
| `components/NameBadge.scala` | NameBadge, BannerLogo, AdminModeToggle | ~95 | ✅ |

### Step 1.3: Extract services ✅

| File | Functions | Lines | Status |
|------|-----------|-------|--------|
| `services/AudioService.scala` | initAudioOnGesture, playVoteSound, celebrateVote | ~60 | ✅ |
| `services/AuthService.scala` | getCookie, deleteCookie, getGitHubUsername, isAccessTokenExpired, refreshAccessToken, fetchTicketWithRefresh, fetchTicketAsync | ~120 | ✅ |

### Step 1.4: Create AppState.scala ✅
Central state management (all the Vars that live in FrontEnd object):
- discussionState, votedTopicOrder, everVotedTopics
- soundMuted, celebratingTopics
- hasSeenSwipeHint, showSwipeHint
- activeDiscussion, popoverState, swapMenuState, etc.

### Step 1.5: Slim down FrontEnd.scala ❌
What remains:
- `object FrontEnd extends App` - entry point
- `val app` - main app composition
- WebSocket setup and event handling
- Imports and wiring

Target: ~400-500 lines

---

## Phase 2: Break up SharedCode.scala (~216 lines → ~6 files)

### Step 2.1: Extract models ❌

| File | Types | Status |
|------|-------|--------|
| `models/Person.scala` | Person newtype | ❌ |
| `models/Vote.scala` | VotePosition, VotingState, Feedback | ❌ |
| `models/Discussion.scala` | Discussion (already has some) | ❌ |
| `models/Room.scala` | Room | ❌ |
| `models/TimeSlot.scala` | TimeSlot, RoomSlot, TimeSlotForAllRooms, DaySlots, ScheduledDiscussion | ❌ |

---

## Phase 3: Quick wins (if not covered above)

- [x] AdminControls extraction (covered in 1.2)
- [x] ToastManager extraction (covered in 1.1)

---

## Execution Order

1. **Create directory structure** (empty packages)
2. **Phase 1.1**: Extract utility objects (no dependencies, safe)
3. **Phase 2**: Extract shared models (foundational)
4. **Phase 1.3**: Extract services (auth, audio)
5. **Phase 1.4**: Create AppState
6. **Phase 1.2**: Extract components (may need AppState imports)
7. **Phase 1.5**: Clean up FrontEnd.scala
8. **Verify**: `sbt compile` after each major step
9. **Test**: Manual testing after completion

---

## Rollback Plan

Each phase should be a separate commit. If something breaks:
```bash
git revert HEAD  # Undo last commit
# or
git reset --hard <last-good-commit>
```

---

## Resume Instructions

If context is lost, read this file and:
1. Check status markers (✅/❌) above
2. Continue from first uncompleted step
3. After each extraction:
   - Update imports in affected files
   - Run `sbt compile` to verify
   - Mark step as ✅
   - Commit with descriptive message

---

## Notes

- Keep package as `co.wtf.openspaces` for all files
- Components that need FrontEnd state will import from `AppState`
- Use `export` statements if needed for backward compatibility
- Preserve all existing functionality - this is pure refactoring
