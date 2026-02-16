# Frontend Simplification Plan

This plan tracks the simplification work requested for Laminar/ScalaJS frontend code.

## Goals
- Reduce manual class string construction and inline style juggling.
- Remove the heat concept entirely from UI logic and CSS.
- Keep strong typing and avoid dynamic JS patterns.
- Improve consistency of reusable UI building blocks.

## Phase 1: Baseline and Heat Removal
- [x] Create persistent plan file.
- [x] Remove heat logic from `TopicCard`.
- [x] Remove heat logic from `VoteButtons`.
- [x] Remove heat CSS selectors and related comments.
- [x] Ensure vote count still has clear styling without heat levels.

## Phase 2: Typed Class Composition
- [x] Add typed class utilities for composing base/modifier classes in Laminar.
- [x] Refactor high-churn components to use helpers:
  - [x] `SvgIcon` / `GitHubAvatar`
  - [x] `TopicCard`
  - [x] `VoteButtons`
  - [x] `ViewToggle`
  - [x] `AdminControls`
  - [x] `NameBadge` sound toggle
  - [x] `ConnectionStatus` indicators/banners
  - [x] `TopicSubmission`

## Phase 3: Menu Consolidation
- [x] Extract shared menu shell (`position`, backdrop behavior, header).
- [x] Extract shared menu primitives (`section`, `topic row`, `action button`, `cancel button`).
- [x] Refactor all menu variants to shared primitives:
  - [x] `Menu`
  - [x] `UnscheduledDiscussionsMenu`
  - [x] `ActiveDiscussionActionMenu`

## Phase 4: Schedule and State Simplification
- [x] Make schedule rendering rely on one source of truth for slot structure.
- [x] Remove hardcoded room header classes (`Room1..Room4`) and derive from room data.
- [x] Add/normalize CSS classes used by schedule slots (`Cell`, empty-slot states).

## Phase 5: Style/CSS Papercuts
- [x] Add missing CSS selectors referenced in Scala (`Menu-textArea`, `Menu-error`, etc.).
- [ ] Remove dead/unused CSS blocks identified during refactor.
- [x] Replace obvious inline style toggles with class-based modifiers where practical.
- [x] Remove remaining `style="..."` in static HTML.

## Validation
- [x] Compile frontend (`sbt` Scala.js/client compile target).
- [ ] Spot-check key UI flows (login, topics, schedule, admin menus).

## Notes
- Keep changes mobile-first.
- Keep websocket-driven model rendering accurate and deterministic.
