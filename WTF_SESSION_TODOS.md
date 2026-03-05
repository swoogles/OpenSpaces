# WTF Session TODOs

UX improvements identified during WTF Scala 2026 sessions.

---

## Accessibility & Clarity

### Don't rely on color alone to convey intent
Currently, vote state (interested vs not interested) is communicated primarily through background color (green vs gray). This fails for colorblind users and isn't immediately obvious to new users. We should supplement color with:
- Clear icons (♥ for interested, ✗ for not interested)
- Text labels where space permits
- Shape/pattern differentiation

### Instructions should not be necessary
Users shouldn't need to be told how to use the app. The UI should communicate affordances through:
- Recognizable interaction patterns (swipe hints, tap targets)
- Progressive disclosure — don't overwhelm, but make next actions obvious
- Consistent behavior across similar components
- Visual cues that suggest interactivity (shadows, borders, hover states)

---

## Schedule View

### "Jump to now" by default
When a user taps the Schedule view button, the view should automatically scroll to the current time slot (or the next upcoming slot if between sessions). Users care most about "what's happening now / next" — make that the default landing spot rather than showing the top of the schedule.

### Gray out past items
Time slots that have already passed should be visually de-emphasized:
- Reduced opacity or desaturated colors
- Possibly collapsed by default with an "expand past sessions" option
- Clear visual separation between "past" and "current/upcoming"
- Helps users focus on what's still actionable

---

## Entity Creation (Plus Button)

### Unified creation entry point
The plus button should be the single launching point for creating *any* entity type:
- Discussion topics
- Activities
- Lightning talk proposals
- Hackathon projects

Instead of having separate creation flows scattered around, the plus button opens a branching menu/component where users first choose what type of entity they want to create.

### Bottom sheet form pattern
All creation forms should emerge from the sticky area at the bottom of the screen:
- Consistent animation and positioning across all entity types
- Unified form structure (header, fields, submit) with type-specific customization
- Forms slide up from bottom, keeping context visible behind
- Dismissible by swipe-down or tap-outside
- Feels native and reduces cognitive load vs full-page navigations
