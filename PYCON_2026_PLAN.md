# PyCon 2026 Open Spaces Adaptation Plan

## Context

**Current state:** StickyIcky is a working Open Spaces app built for Winter Tech Forum (WTF) — a ~50-100 person conference. It handles topic submission, interest voting, conflict-aware scheduling, room assignment, and real-time sync via WebSockets.

**Target:** PyCon US 2026 Open Spaces — a subset of PyCon's larger conference. Open Spaces run during the 3 main conference days alongside talks.

## PyCon 2026 Event Details

- **Dates:** May 13-19, 2026
- **Location:** Long Beach Convention Center, Long Beach, California
- **Full Schedule:**
  - Tutorials: May 13-14
  - Sponsor Presentations: May 14-15
  - **Main Conference: May 15-17** ← Open Spaces happen here
  - Job Fair / Community Showcase: May 17
  - Sprints: May 18-19

### Open Spaces Specifics (Based on 2025 Data)

- **Duration:** 3 days (Friday-Sunday) during Main Conference
- **Time Slots:** 1-hour intervals, roughly 11am-6pm+ each day (~6-7 slots/day)
- **Rooms:** ~7-8 dedicated rooms (2025 used: 308, 309, 315, 316, 318, 320, 321, plus specialty spaces)
- **Total capacity:** ~120-170 session slots across the event (7 rooms × 6 slots × 3 days ≈ 126)
- **Attendance:** Unknown percentage of ~2,000-3,000 attendees engage with Open Spaces
- **Existing System:** PyCon already uses a **web-based signup system** on us.pycon.org
  - Signups open day-before for each day
  - Can submit up to 1 hour before slot time
  - Shows current schedule with room/time grid

### What PyCon's Current System Does
- Topic submission with title and description
- Room + time slot assignment (self-service)
- Browse schedule by day
- Mobile-friendly web interface

**Screenshots saved for reference:**
- Schedule view: [docs/pycon-reference/pycon-2025-schedule.jpg](docs/pycon-reference/pycon-2025-schedule.jpg)
- Signup grid: [docs/pycon-reference/pycon-2025-signup-grid.jpg](docs/pycon-reference/pycon-2025-signup-grid.jpg)

### What It May NOT Do (Opportunities for StickyIcky)
- **Interest voting / popularity tracking** — no "I want to attend this" feature visible
- **Conflict detection** — no warning when you're interested in overlapping sessions  
- **Auto-scheduling** — appears to be first-come-first-served room selection
- **Real-time sync** — unclear if live updates or page refresh needed
- **Room capacity matching** — no visible "this topic is too popular for this room" intelligence

## Key Differences: WTF vs PyCon Open Spaces

| Dimension | WTF (Current) | PyCon (Target) | Impact |
|-----------|--------------|----------------|--------|
| Attendees | ~50-100 | ~2,000-3,000 | Scale everything |
| Concurrent users | ~30-50 | ~200-500? | WebSocket architecture |
| Topics submitted | ~20-50 | ~100-200? | Discovery/filtering helpful |
| Rooms | 4-5 | 7-8 | Similar complexity |
| Days | 2-3 | 3 (Open Spaces only) | Comparable |
| Auth | Name-only (trust-based) | TBD | Identity/moderation needs |
| Existing tooling | Greenfield | Has existing system | Integration/replacement decision |

---

## StickyIcky's Key Differentiators

What StickyIcky does that PyCon's current system apparently doesn't:

1. **Interest Voting** — Know which topics have demand before scheduling
2. **Conflict Detection** — Algorithm considers "who wants to attend both A and B?"
3. **Smart Scheduling** — Auto-assign rooms based on interest + capacity + conflicts
4. **Real-time Sync** — WebSockets mean everyone sees changes instantly
5. **Room Capacity Matching** — High-interest topics get bigger rooms
6. **Slack Integration** — Discussion threads per topic
7. **"Schedule evolves"** — Late submissions can earn slots if popular

These are meaningful improvements for attendee experience. The question is whether they're compelling enough for PyCon to switch systems or adopt as augmentation.

---

## Workstreams

### 1. Scale & Performance

**WebSocket Infrastructure**
- [ ] Load test current implementation — what breaks at 500+ concurrent connections?
- [ ] Consider connection pooling / fan-out architecture
- [ ] Evaluate Redis pub/sub for horizontal scaling (multiple server instances)
- [ ] Implement connection limits and graceful degradation
- [ ] Add metrics/observability (connection count, message rate, latency)

**Database**
- [ ] Review query patterns under load
- [ ] Add indexes for common queries (topics by interest, schedule lookups)
- [ ] Consider read replicas if needed
- [ ] Connection pool sizing

**Deployment**
- [ ] Heroku → more scalable option? (Railway, Fly.io, k8s)
- [ ] CDN for static assets
- [ ] Geographic distribution considerations (PyCon location)

### 2. Discovery & Navigation

With 200-500 topics, users can't scroll through everything.

**Search & Filter**
- [ ] Full-text search on topic titles/descriptions
- [ ] Filter by: scheduled/unscheduled, my interests, time slot, room
- [ ] Tags/categories for topics
- [ ] "Recommended for you" based on voting patterns (stretch goal)

**Topic Descriptions**
- [ ] Add description field (currently title-only)
- [ ] Rich text or markdown support?
- [ ] Character limits (title: 100? description: 500?)

**Information Architecture**
- [ ] Clearer separation: Browse topics vs My interests vs Schedule
- [ ] Quick-access to "happening now" and "next up"
- [ ] Day-by-day navigation improvements

### 3. Identity & Moderation

WTF's "just enter your name" won't scale to PyCon.

**Authentication Options**
- [ ] PyCon attendee integration (if API exists)
- [ ] Email magic links
- [ ] OAuth (GitHub, Google)
- [ ] QR code from badge (physical → digital link)

**Moderation Needs**
- [ ] Report inappropriate content
- [ ] Admin queue for flagged items
- [ ] Ability to merge duplicate topics
- [ ] Topic ownership verification
- [ ] Rate limiting (topic creation, voting)

**Roles**
- [ ] Attendee (submit, vote, view)
- [ ] Facilitator (same as attendee, marked differently)
- [ ] Volunteer (moderation capabilities)
- [ ] Admin (full control)

### 4. Scheduling Algorithm Enhancements

Current algorithm is solid but needs refinement for scale.

**Algorithm Improvements**
- [ ] Better handling of 500+ topics competing for 50-100 slots
- [ ] "Popularity threshold" — minimum interest to auto-schedule
- [ ] Manual vs auto scheduling modes
- [ ] Scheduler run frequency at scale (every 5 min? on-demand?)

**Room Capacity Intelligence**
- [ ] Better capacity matching (huge interest → largest room)
- [ ] Overflow handling (standby, second session?)
- [ ] Room location/clustering (related topics near each other?)

**Conflict Visibility**
- [ ] Show users their personal schedule conflicts
- [ ] "You're interested in 3 things at 2pm" warning
- [ ] Suggest alternatives

### 5. UI/UX for Scale

**Mobile-First Refinements**
- [ ] Performance with 500+ topics in list
- [ ] Virtual scrolling / pagination
- [ ] Offline resilience (PWA improvements)

**Schedule View**
- [ ] Calendar/grid view for multi-day overview
- [ ] Export to personal calendar (ics)
- [ ] "My schedule" personalized view

**Notifications**
- [ ] "Your session is scheduled!" 
- [ ] "Starting in 15 minutes"
- [ ] Push notifications (PWA or optional SMS?)

### 6. PyCon-Specific Features

**Open Spaces Traditions**
- [ ] Understand PyCon Open Spaces culture/norms
- [ ] "Law of Two Feet" awareness (explain in UI?)
- [ ] Historical session data (past PyPyCons)?

**Physical/Digital Bridge**
- [ ] Room maps / wayfinding
- [ ] Physical signage integration (QR codes per room?)
- [ ] Attendance tracking (optional, for metrics)

**Community Integration**
- [ ] Slack/Discord integration (PyCon channels)
- [ ] Link to speaker/attendee profiles
- [ ] Post-session feedback/notes

### 7. Resilience & Operations

**Failure Modes**
- [ ] What happens when the app goes down during conference?
- [ ] Graceful degradation to static schedule
- [ ] Manual fallback procedures documented

**Monitoring**
- [ ] Real-time dashboard for organizers
- [ ] Alerting on key metrics
- [ ] Error tracking (Sentry or similar)

---

## Strategic Question: Replace or Augment?

PyCon already has a working digital Open Spaces system. Two paths forward:

### Option A: Replace Their System
- Pitch StickyIcky as a superior alternative
- Would need buy-in from PyCon organizers
- Higher stakes, higher impact
- Requires proving value over existing solution

### Option B: Augment / Layer On Top
- Build as a companion app
- Users can discover topics, express interest, see conflicts
- Doesn't control actual scheduling (that stays with PyCon's system)
- Lower friction adoption — no organizer approval needed
- Could prove value for future full adoption

### Option C: Use PyCon as Forcing Function
- Build the features (search, scale, etc.) for StickyIcky generally
- Make them good enough to pitch to any conference
- PyCon becomes a target use case, not the only use case

---

## Open Questions (Need Input)

### Strategic
1. **Which path: Replace, Augment, or general improvement?**
2. Do you have any contact with PyCon organizers?
3. Is there interest from them in trying something new?

### From PyCon Organizers (if pursuing Replace path)
1. Pain points with current system?
2. Would they consider an alternative?
3. Infrastructure constraints? (Must run on PSF infra?)
4. Data/privacy requirements?

### Architectural Decisions
1. ~~Keep Scala/ZIO stack or consider Python rewrite?~~ **Decided: Keep Scala**
2. Self-hosted vs PyCon infrastructure integration?
3. Data retention — save across years?

### Product Decisions
1. Anonymous topic submission allowed, or require identity?
2. Voting visible to all, or private?
3. Who can modify the schedule? (Admins only, or emergent/democratic?)
4. Integration with main PyCon schedule/app?

---

## Phased Delivery

### Phase 1: Mainline Improvements (General App Work)
These make StickyIcky better for any conference:

- [ ] **Search and filter** — critical for 100+ topics
- [ ] Topic descriptions (not just titles)
- [ ] Load testing — find breaking points
- [ ] Conflict visibility for users ("You're interested in 3 overlapping sessions")
- [ ] Calendar export (ics)
- [ ] "My schedule" personalized view

### Phase 2: Scale Hardening
- [ ] WebSocket architecture review for 500+ concurrent
- [ ] Database query optimization + indexes
- [ ] Production monitoring / observability
- [ ] Connection limits + graceful degradation

### Phase 3: PyCon-Specific (If Pursuing)
- [ ] Auth integration (PyCon attendee system, or standalone)
- [ ] Room configuration for Long Beach Convention Center
- [ ] Time slot configuration (May 15-17, 2026 schedule)
- [ ] Admin/volunteer moderation tools
- [ ] Notifications (session starting soon, your topic got scheduled)

### Phase 4: Polish
- [ ] Performance tuning under load
- [ ] Accessibility audit
- [ ] Mobile UX refinements for conference environment
- [ ] Volunteer training docs

---

## Risks

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Scale issues discovered late | Medium | High | Early load testing |
| PyCon uses different tooling | Medium | High | Early organizer contact |
| WebSocket limits at scale | Medium | High | Architect for horizontal scale early |
| Moderation overwhelm | Medium | Medium | Rate limits, spam detection |
| Auth integration complexity | High | Medium | Start simple, iterate |

---

## Timeline

**PyCon US 2026:** May 15-17, 2026 (Open Spaces portion)
**Today:** March 7, 2026
**Time remaining:** ~10 weeks

If targeting PyCon 2026, this is tight but doable for focused improvements.

### Key Milestones (Suggested)
- **March:** Search implementation, load testing, identify scale issues
- **April:** Scale fixes, UI improvements, testing with WTF-scale load
- **Early May:** Final polish, documentation, deployment prep
- **May 13:** Ready for PyCon (if pursuing)

---

## Next Steps

1. **Decide strategic path** — Replace/Augment/General improvement?
2. **Load test current app** — find breaking points
3. **Implement search** — as mainline work
4. **(If pursuing PyCon)** Contact organizers — understand interest, requirements
5. **Scale testing** — simulate 500+ concurrent users

---

## Notes

*This is a living document. Update as we learn more from PyCon organizers and testing.*

---

*Created: 2026-03-07*
