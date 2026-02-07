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

## Architecture

### Where Slack calls hook in

The natural integration point is `PersistentDiscussionStore.applyAction` (or a wrapper around it). After the existing persist-and-broadcast logic succeeds, a `SlackNotifier` service fires the corresponding Slack API call. This keeps Slack as a side-effect that doesn't block or fail the core action.

```
DiscussionAction
  -> PersistentDiscussionStore.applyAction  (persist + WebSocket broadcast)
  -> SlackNotifier.notify                   (fire-and-forget Slack API call)
```

The `SlackNotifier` should be called from `DiscussionService.handleTicketedAction`, after `applyAction` succeeds and after WebSocket broadcast. This way:
- Core logic is unaffected if Slack is down
- The `SlackNotifier` has access to the `DiscussionActionConfirmed` result (which includes the full `Discussion` for `AddResult`)

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

Orchestrates which Slack action to take based on the `DiscussionActionConfirmed`. Runs as a fire-and-forget fiber so it doesn't block WebSocket broadcast.

**File:** `server/src/main/scala/co/wtf/openspaces/slack/SlackNotifier.scala`

**Handles these confirmed actions:**
| Action | Slack behavior |
|---|---|
| `AddResult(discussion)` | Post new message to channel. Store resulting `ts`. Fetch permalink, store it. |
| `Rename(topicId, newTopic)` | Look up stored `ts`, call `chat.update` with new topic name. |
| `UpdateRoomSlot(topicId, roomSlot)` | Look up stored `ts`, call `chat.update` adding room/time info. |
| `Unschedule(topicId)` | Look up stored `ts`, call `chat.update` removing room/time info. |
| `Delete(topicId)` | Look up stored `ts`, post "This topic has been cancelled" reply. |
| `MoveTopic`, `SwapTopics` | Look up stored `ts`(s), call `chat.update` with new room/time. |

All other actions (Vote, RemoveVote) are no-ops for Slack.

#### 3. `SlackThreadRepository` (server-side)

Persistence layer for the Slack thread mapping.

**File:** `server/src/main/scala/co/wtf/openspaces/slack/SlackThreadRepository.scala`

```scala
trait SlackThreadRepository:
  def save(topicId: Long, channelId: String, threadTs: String, permalink: String): Task[Unit]
  def findByTopicId(topicId: Long): Task[Option[SlackThreadRef]]

case class SlackThreadRef(channelId: String, threadTs: String, permalink: String)
```

### Database changes

**New migration: `V2__slack_threads.sql`**

```sql
CREATE TABLE slack_threads (
    topic_id BIGINT PRIMARY KEY REFERENCES discussions(id),
    channel_id TEXT NOT NULL,
    thread_ts TEXT NOT NULL,       -- Slack's message timestamp (unique ID)
    permalink TEXT NOT NULL,       -- Pre-fetched deep link URL
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_slack_threads_topic ON slack_threads(topic_id);
```

Separate table rather than adding columns to `discussions` because:
- Slack is optional -- the app should work fine without it
- Clean separation of concerns
- Easy to drop/rebuild without touching core data

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

This is populated when loading discussions from the database (join with `slack_threads` table). It flows to the client via WebSocket as part of `DiscussionActionConfirmed.AddResult`.

**Important:** Since the Slack API call is async (fire-and-forget), the `slackThreadUrl` won't be available in the initial `AddResult` broadcast. Options:
- **Option A:** Wait for the Slack API response before broadcasting (adds latency, ~200-500ms). The Slack link appears immediately on all clients.
- **Option B:** Broadcast `AddResult` immediately without the link. After the Slack call completes, broadcast a new `SlackThreadLinked(topicId, url)` action to all clients. The link appears after a brief delay.
- **Recommended: Option A.** The latency is acceptable for topic creation (it's not a frequent action), and it keeps the protocol simpler. If Slack is unreachable, fall back to broadcasting without the link and retry in the background.

### Frontend changes

**File:** `client/src/main/scala/co/wtf/openspaces/FrontEnd.scala`

In `SingleDiscussionComponent`, add a Slack link icon/button in the `SecondaryActive` area (alongside facilitator name and room slot):

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
📍 King · 9:00-9:50
```

On **delete**, a threaded reply is posted:
```
🚫 This topic has been cancelled.
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

- Slack API calls are fire-and-forget ZIO fibers
- On failure, log a warning and schedule a retry (up to 3 attempts with exponential backoff: 1s, 5s, 15s)
- If all retries fail, log an error but do not fail the topic action
- The `slack_threads` row is only written after a successful Slack API response
- If the app restarts and a discussion has no `slack_threads` row, the Slack link simply won't appear on that card (acceptable degradation)

### Channel lockdown

This is a **Slack workspace admin configuration step**, not code:

1. Go to `#topic-discussion` channel settings
2. Under "Posting permissions", set to "Admins and specific people only" (or "Channel managers only")
3. Add the bot app to the channel
4. The bot can post top-level messages; humans can only reply to existing threads

Document this in a setup guide. No code needed.

## Implementation order

### Step 1: Database & config
- [ ] Add `V2__slack_threads.sql` migration
- [ ] Create `SlackConfig` with env var loading
- [ ] Wire `SlackConfig` into the ZIO dependency graph as optional

### Step 2: Slack client
- [ ] Implement `SlackClient` using `zio-http` Client
- [ ] Handle auth header (`Authorization: Bearer xoxb-...`)
- [ ] Parse Slack API responses (they return `{"ok": true/false, ...}`)
- [ ] Add retry logic

### Step 3: Slack notifier
- [ ] Implement `SlackNotifier` with handlers for each action type
- [ ] Implement `SlackThreadRepository` for persistence
- [ ] Wire into `DiscussionService.handleTicketedAction`

### Step 4: Shared model
- [ ] Add `slackThreadUrl: Option[String]` to `Discussion`
- [ ] Update `PersistentDiscussionStore.loadInitialState` to join with `slack_threads`
- [ ] Update `createDiscussion` flow to call Slack before broadcasting

### Step 5: Frontend
- [ ] Add Slack icon asset
- [ ] Add Slack link to `SingleDiscussionComponent`
- [ ] Style the link

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
