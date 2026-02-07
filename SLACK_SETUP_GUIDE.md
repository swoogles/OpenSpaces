# Slack App Setup Guide

## 1. Create the Slack App

1. Go to https://api.slack.com/apps
2. Click **"Create New App"**
3. Choose **"From scratch"**
4. Name it something like **"OpenSpaces"**
5. Select your workspace
6. Click **"Create App"**

## 2. Configure Bot Token Scopes

1. In the left sidebar, go to **"OAuth & Permissions"**
2. Scroll down to **"Scopes" > "Bot Token Scopes"**
3. Click **"Add an OAuth Scope"** and add these two scopes:
   - `chat:write` — lets the bot post and update messages
   - `channels:read` — lets the bot verify the channel exists

## 3. Install the App to Your Workspace

1. Scroll back up on the same **"OAuth & Permissions"** page
2. Click **"Install to Workspace"**
3. Review the permissions and click **"Allow"**
4. Copy the **"Bot User OAuth Token"** — it starts with `xoxb-`. You'll need this for the `SLACK_BOT_TOKEN` env var.

## 4. Create the Channel

1. In Slack, create a new channel called **`#topic-discussion`** (or whatever name you prefer)
2. Once created, you need the **channel ID** (not the name):
   - Right-click the channel name in the sidebar
   - Click **"View channel details"**
   - The channel ID is at the bottom of the details panel — it looks like `C01ABCDEF23`
   - Copy this for the `SLACK_CHANNEL_ID` env var

## 5. Invite the Bot to the Channel

In the `#topic-discussion` channel, type:

```
/invite @OpenSpaces
```

(Use whatever name you gave the app in step 1.) The bot must be in the channel to post messages.

## 6. Lock Down Channel Posting (Optional but Recommended)

This ensures only the bot posts top-level messages and humans can only reply in threads:

1. Open `#topic-discussion` channel settings (click the channel name at the top)
2. Go to **"Settings"** tab
3. Under **"Posting permissions"**, set to **"Admins and specific people only"** (or "Channel managers only")
4. The bot app can still post because it uses the API, but regular users will only be able to reply to existing threads

## 7. Set Environment Variables

Set these three env vars wherever you run the server (Heroku, local `.env`, etc.):

```bash
SLACK_BOT_TOKEN=xoxb-your-token-here
SLACK_CHANNEL_ID=C01ABCDEF23
APP_BASE_URL=https://your-app.herokuapp.com
```

- `SLACK_BOT_TOKEN` — the `xoxb-` token from step 3
- `SLACK_CHANNEL_ID` — the channel ID from step 4
- `APP_BASE_URL` — your app's public URL (no trailing slash). This is used in the "View in OpenSpaces" link in Slack messages.

If any of these three are missing, the integration silently disables itself — the app works normally, just without Slack posts.

## 8. Verify It Works

1. Start/restart the server — you should see the log line: `Slack integration enabled for channel C01ABCDEF23`
2. Create a new topic in OpenSpaces
3. Within a few seconds, a message should appear in `#topic-discussion` with the topic name, facilitator avatar, and a "View in OpenSpaces" link
4. The topic card in the app should show a small Slack icon linking to that thread
