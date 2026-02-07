# Slack Integration Plan

## Overview

Integrate OpenSpaces with Slack so that each topic automatically gets a dedicated thread in a `#topic-discussion` channel. The app is the system of record; Slack serves as a discussion sidecar. Data flows **one-way: app -> Slack**.

## Decisions Made

| Question | Decision |
|---|---|
| User identity mapping | No mapping. Slack identities are independent of GitHub auth. |
| Data flow direction | One-way (app -> Slack). No Slack -> app sync for now. |
| Topic rename | Update the original Slack message. |
| Topic schedule | Update the original Slack message with room/time. |
| Topic delete | Post a "This topic has been cancelled" reply in the thread. |
| Initial message content | Topic name, facilitator name, facilitator avatar, link back to app. No vote count. |
| Schema approach | Add Slack columns directly to `discussions` table (nullable). |
| Failure handling | Create topic immediately, queue Slack post for background retry via ZIO. |
| Channel strategy | Single `#topic-discussion` channel for all events. |
| Thread link visibility | Always show the link, even if no replies yet -- steer people toward Slack. |
| Creation latency | Zero-latency: broadcast `AddResult` immediately, then send `SlackThreadLinked` once Slack responds. |

## Architecture

### Where Slack calls hook in

The natural integration point is `DiscussionService.handleTicketedAction`. After `applyAction` succeeds and the `AddResult` is broadcast to all WebSocket clients, a `SlackNotifier` fires the Slack API call on a background ZIO fiber. When the Slack call succeeds, a `SlackThreadLinked` message is broadcast to all clients so the link appears on topic cards.

```
DiscussionAction.Add
  -> PersistentDiscussionStore.applyAction   (persist + return confirmed)
  -> broadcast AddResult to all clients      (immediate, no Slack link yet)
  -> SlackNotifier.notify (forked fiber)
       -> Slack API: chat.postMessage
       -> Slack API: chat.getPermalink
       -> persist slack_channel_id, slack_thread_ts, slack_permalink on discussions row
       -> broadcast SlackThreadLinked(topicId, permalink) to all clients
```

For other actions (rename, schedule, delete), the Slack update is purely fire-and-forget -- no new WebSocket message type needed since the Slack thread URL doesn't change.

### New WebSocket message type

**File:** `shared_code/shared/src/main/scala/co/wtf/openspaces/DiscussionAction.scala`

Add to `DiscussionActionConfirmed`:

```scala
case SlackThreadLinked(topicId: TopicId, slackThreadUrl: String)
```

The frontend handles this by updating the `Discussion` in its local state to set `slackThreadUrl`. This means the link appears on the topic card a moment after creation, without any page reload.

### New components

#### 1. `SlackClient` (server-side)

A thin ZIO wrapper around the Slack Web API. Uses `zio-http`'s existing HTTP client (already a dependency) -- no new HTTP library needed.

**Required Slack API methods:**
- `chat.postMessage` -- post the initial thread message when a topic is created
- `chat.update` -- edit the original message on rename or schedule
- `chat.postMessage` (with `thread_ts`) -- post a reply on delete ("cancelled")
- `chat.getPermalink` -- get the deep link URL for the topic card

**File:** `server/src/main/scala/co/wtf/openspaces/slack/SlackClient.scala`

```scala
trait SlackClient:
  def postMessage(channel: String, blocks: String): Task[SlackMessageRef]
  def updateMessage(channel: String, ts: String, blocks: String): Task[Unit]
  def postReply(channel: String, threadTs: String, text: String): Task[Unit]
  def getPermalink(channel: String, messageTs: String): Task[String]

case class SlackMessageRef(channel: String, ts: String)
```

#### 2. `SlackNotifier` (server-side)

Orchestrates which Slack action to take based on the `DiscussionActionConfirmed`. For `AddResult`, it runs on a forked fiber and broadcasts `SlackThreadLinked` on success. For other actions, it's fire-and-forget.

**File:** `server/src/main/scala/co/wtf/openspaces/slack/SlackNotifier.scala`

**Handles these confirmed actions:**
| Action | Slack behavior |
|---|---|
| `AddResult(discussion)` | Post new message to channel. Store resulting `ts` + permalink on `discussions` row. Broadcast `SlackThreadLinked` to all WebSocket clients. |
| `Rename(topicId, newTopic)` | Look up stored `ts`, call `chat.update` with new topic name. |
| `UpdateRoomSlot(topicId, roomSlot)` | Look up stored `ts`, call `chat.update` adding room/time info. |
| `Unschedule(topicId)` | Look up stored `ts`, call `chat.update` removing room/time info. |
| `Delete(topicId)` | Look up stored `ts`, post "This topic has been cancelled" reply. |
| `MoveTopic`, `SwapTopics` | Look up stored `ts`(s), call `chat.update` with new room/time. |

All other actions (Vote, RemoveVote) are no-ops for Slack.

The `SlackNotifier` needs access to:
- `SlackClient` for API calls
- `DiscussionRepository` to read/write `slack_*` columns
- `Ref[List[OpenSpacesServerChannel]]` (the connected users ref from `DiscussionService`) to broadcast `SlackThreadLinked`

### Database changes

**New migration: `V2__slack_thread_columns.sql`**

```sql
-- Add Slack thread tracking columns to discussions table.
-- Nullable because Slack integration is optional and because
-- the Slack post happens asynchronously after topic creation.
ALTER TABLE discussions
    ADD COLUMN slack_channel_id TEXT,
    ADD COLUMN slack_thread_ts TEXT,
    ADD COLUMN slack_permalink TEXT;
```

These columns are nullable because:
- Slack integration is optional (local dev, tests, or environments without Slack configured)
- The Slack post happens asynchronously -- the row exists before the Slack API responds
- Existing rows from before the migration won't have Slack data

**Update `DiscussionRepository`** to read/write these columns. Add a method:

```scala
def updateSlackThread(topicId: Long, channelId: String, threadTs: String, permalink: String): Task[Unit]
```

**Update `DiscussionRow`** to include the new fields:

```scala
case class DiscussionRow(
  // ... existing fields ...
  slackChannelId: Option[String],
  slackThreadTs: Option[String],
  slackPermalink: Option[String]
)
```

### Shared model changes

**File:** `shared_code/shared/src/main/scala/co/wtf/openspaces/SharedCode.scala`

Add an optional `slackThreadUrl` field to `Discussion`:

```scala
case class Discussion(
  topic: Topic,
  facilitator: Person,
  interestedParties: Set[Feedback],
  id: TopicId,
  glyphicon: Glyphicon,
  roomSlot: Option[RoomSlot],
  facilitatorDisplayName: Option[String],
  slackThreadUrl: Option[String]       // <-- new field
) derives JsonCodec
```

This is populated when loading discussions from the database (from the `slack_permalink` column). It flows to the client via WebSocket as part of `DiscussionActionConfirmed.AddResult`.

On initial load, discussions that already have Slack threads will have the URL populated. For newly created topics, the URL arrives via a separate `SlackThreadLinked` message moments later.

### Frontend changes

**File:** `client/src/main/scala/co/wtf/openspaces/FrontEnd.scala`

**Handle `SlackThreadLinked`:** In the WebSocket message handler, when a `SlackThreadLinked(topicId, url)` arrives, update the corresponding `Discussion` in client-side state to set its `slackThreadUrl`.

**Render the link in `SingleDiscussionComponent`:** Add a Slack link icon/button in the `SecondaryActive` area (alongside facilitator name and room slot):

```scala
// Inside the SecondaryActive div
topic.slackThreadUrl match
  case Some(url) =>
    a(
      href := url,
      target := "_blank",
      cls := "SlackThreadLink",
      title := "Discuss in Slack",
      img(src := "/icons/slack.svg", cls := "SlackIcon"),
    )
  case None => emptyNode
```

**Styling** (`server/src/main/resources/public/styles.css`):
- Small Slack icon (~16x16) next to facilitator name
- Subtle, doesn't compete with vote buttons
- Hover state with tooltip "Discuss in Slack"

### Slack message format

Use Slack Block Kit for the initial message. Example structure:

```json
{
  "blocks": [
    {
      "type": "section",
      "text": {
        "type": "mrkdwn",
        "text": "*Continuous Deployment - A goal, an asymptote, or an ass out of you and me?*"
      },
      "accessory": {
        "type": "image",
        "image_url": "https://github.com/swoogles.png?size=100",
        "alt_text": "swoogles"
      }
    },
    {
      "type": "context",
      "elements": [
        {
          "type": "mrkdwn",
          "text": "Proposed by *swoogles* · <https://your-app.herokuapp.com|View in OpenSpaces>"
        }
      ]
    }
  ]
}
```

On **rename**, the section text block is updated with the new topic name.

On **schedule**, a new context element is appended:
```
:round_pushpin: King · 9:00-9:50
```

On **delete**, a threaded reply is posted:
```
:no_entry_sign: This topic has been cancelled.
```

### Configuration

**New environment variables:**

| Variable | Description | Example |
|---|---|---|
| `SLACK_BOT_TOKEN` | Bot User OAuth Token (`xoxb-...`) | `xoxb-123-456-abc` |
| `SLACK_CHANNEL_ID` | Target channel ID for `#topic-discussion` | `C01ABCDEF23` |
| `APP_BASE_URL` | Public URL of the app (for "View in OpenSpaces" links) | `https://your-app.herokuapp.com` |

**File:** `server/src/main/scala/co/wtf/openspaces/slack/SlackConfig.scala`

```scala
case class SlackConfig(
  botToken: String,
  channelId: String,
  appBaseUrl: String
)

object SlackConfig:
  def fromEnv: Task[Option[SlackConfig]] = // Returns None if vars not set
```

When `SlackConfig` is `None`, the `SlackNotifier` becomes a no-op. This keeps the app functional in environments without Slack configured (local dev, tests).

### Error handling & retry

Slack API calls run on background ZIO fibers, completely decoupled from the topic action pipeline:

- **Topic creation flow:** `applyAction` persists and broadcasts `AddResult` immediately. A ZIO fiber is forked to handle the Slack post. If it fails, ZIO's `Schedule.exponential` handles retries (up to 3 attempts: 1s, 5s, 15s backoff).
- **On success:** The fiber writes `slack_channel_id`, `slack_thread_ts`, and `slack_permalink` to the `discussions` row, then broadcasts `SlackThreadLinked` to all connected clients.
- **On final failure:** Log an error. The topic exists in the app without a Slack link. No user-facing error. An admin can investigate and manually retry or create the thread.
- **On app restart:** Discussions loaded from the database will have `slackThreadUrl = None` if the Slack post never succeeded. The link simply won't appear on that card (acceptable degradation). A future enhancement could detect these orphaned rows and retry on startup.

```scala
// Pseudocode for the retry logic in SlackNotifier
val retryPolicy = Schedule.exponential(1.second) && Schedule.recurs(2) // 1s, 2s, 4s (3 attempts total)

slackClient.postMessage(channelId, blocks)
  .retry(retryPolicy)
  .flatMap { ref =>
    slackClient.getPermalink(channelId, ref.ts)
      .retry(retryPolicy)
      .flatMap { permalink =>
        discussionRepo.updateSlackThread(topicId, channelId, ref.ts, permalink) *>
        broadcastSlackThreadLinked(topicId, permalink)
      }
  }
  .catchAll(err => ZIO.logError(s"Slack integration failed for topic $topicId: $err"))
  .fork // fire-and-forget
```

### Channel lockdown

This is a **Slack workspace admin configuration step**, not code:

1. Go to `#topic-discussion` channel settings
2. Under "Posting permissions", set to "Admins and specific people only" (or "Channel managers only")
3. Add the bot app to the channel
4. The bot can post top-level messages; humans can only reply to existing threads

Document this in a setup guide. No code needed.

## Implementation order

### Step 1: Shared model + WebSocket protocol
- [ ] Add `slackThreadUrl: Option[String]` to `Discussion` (with `None` default for backward compat)
- [ ] Add `SlackThreadLinked(topicId, slackThreadUrl)` to `DiscussionActionConfirmed`
- [ ] Update `Discussion` companion object constructors to include `slackThreadUrl = None`

### Step 2: Database & config
- [ ] Add `V2__slack_thread_columns.sql` migration
- [ ] Update `DiscussionRow` with nullable `slack_channel_id`, `slack_thread_ts`, `slack_permalink`
- [ ] Add `updateSlackThread` method to `DiscussionRepository`
- [ ] Update `PersistentDiscussionStore.loadInitialState` to populate `slackThreadUrl` from `slack_permalink`
- [ ] Create `SlackConfig` with env var loading
- [ ] Wire `SlackConfig` into the ZIO dependency graph as optional

### Step 3: Slack client
- [ ] Implement `SlackClient` using `zio-http` Client
- [ ] Handle auth header (`Authorization: Bearer xoxb-...`)
- [ ] Parse Slack API responses (they return `{"ok": true/false, ...}`)
- [ ] Implement Block Kit message formatting for topic creation, rename, schedule, and delete

### Step 4: Slack notifier
- [ ] Implement `SlackNotifier` with handlers for each action type
- [ ] Wire retry logic using `ZIO.retry` with `Schedule.exponential`
- [ ] On successful `AddResult` Slack post: persist Slack columns, broadcast `SlackThreadLinked`
- [ ] Wire into `DiscussionService.handleTicketedAction` (fork after broadcast)
- [ ] Implement no-op behavior when `SlackConfig` is `None`

### Step 5: Frontend
- [ ] Add Slack icon asset (`/icons/slack.svg`)
- [ ] Handle `SlackThreadLinked` in WebSocket message handler (update discussion state)
- [ ] Add Slack link to `SingleDiscussionComponent` in the `SecondaryActive` area
- [ ] Style the link (subtle icon, hover tooltip)

### Step 6: Documentation & deployment
- [ ] Document Slack App creation steps (scopes, bot token, channel setup)
- [ ] Document channel lockdown steps
- [ ] Add env vars to Heroku config
- [ ] Test end-to-end

## Slack App setup guide (for reference during implementation)

1. Go to https://api.slack.com/apps and create a new app
2. Under "OAuth & Permissions", add these Bot Token Scopes:
   - `chat:write` -- post and update messages
   - `channels:read` -- verify channel exists
3. Install the app to your workspace
4. Copy the "Bot User OAuth Token" (`xoxb-...`)
5. Invite the bot to `#topic-discussion`: `/invite @YourBotName`
6. Set channel posting permissions (see "Channel lockdown" above)

## Out of scope (future consideration)

- Two-way sync (Slack replies shown in app)
- Slack emoji reactions mapped to votes
- Multiple channels for multiple events
- Slash commands in Slack to create topics
- Notifications when topics are scheduled to a room
