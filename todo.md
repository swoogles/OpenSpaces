# OpenSpaces - Future Improvements

## Usability Enhancements

### Interest & Voting
- [x] **Interest heat map** — Visually indicate popularity of sessions (color intensity, size, or badge count)
- [ ] **Room capacity display** — Show capacity per room so attendees know if a session might be packed
- [ ] **"My Schedule" view** — Filter to only show sessions I've voted interested in
- [ ] **Conflict detection** — Warn when interested in overlapping sessions

### Discovery & Navigation
- [ ] **Topic search/filter** — As topics grow, finding a specific one becomes tedious
- [ ] **Topic descriptions** — Allow facilitators to add more than just a title

### Data Management
- [ ] **Event sourcing** — Log all actions for audit trail and replay capability
- [ ] **Export/import** — Backup and restore schedule (JSON export)

## Nice to Have

- [ ] **Undo support** — Allow reverting accidental moves/deletes
- [ ] **Push notifications** — Alert when a session you're interested in gets scheduled
- [ ] **Room details/maps** — Show where rooms are located (directions, floor, etc.)
- [ ] **Facilitator contact** — Optional way to reach session facilitator

## Technical Debt

- [ ] **Configurable schedule** — Move dates/rooms/time slots to config file instead of code
- [ ] **Better error handling** — Surface WebSocket disconnects and reconnection status to users
