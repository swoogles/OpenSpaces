# OpenSpaces - Future Improvements

## Usability Enhancements

### Interest & Voting
- [x] **Interest heat map** — Visually indicate popularity of sessions (color intensity, size, or badge count)
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
- [ ] **Show topic creator soft limit length** — Indicate to them how much of their title will be shown by default on a card without needing to expand it. But then let them enter a longer title if they choose.

## Technical Debt

- [x] **Better error handling** — Surface WebSocket disconnects and reconnection status to users
  - [x] Connection status indicator (dot in header)
  - [x] Disconnect/reconnecting banner with retry button
  - [x] Offline detection (navigator.onLine + events)
  - [x] State sync on reconnect (re-fetch full state when reconnecting)

  - [x] I have given the Slack app the channels:manage Permission. I want it to Create the channel that it plans to use. For the time being- add a `-test` to the channel name and account for that everywhere.
    - Changed `SLACK_CHANNEL_ID` to `SLACK_CHANNEL_NAME` (defaults to `openspaces-discussions-test`)
    - App finds existing channel or creates it on startup
    - Added `findChannelByName` and `createChannel` to SlackClient 