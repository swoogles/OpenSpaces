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
3. Click **"Add an OAuth Scope"** and add these scopes:
   - `chat:write` — lets the bot post and update messages
   - `channels:read` — lets the bot find existing channels
   - `channels:manage` — lets the bot create the channel if it doesn't exist

## 3. Install the App to Your Workspace

1. Scroll back up on the same **"OAuth & Permissions"** page
2. Click **"Install to Workspace"**
3. Review the permissions and click **"Allow"**
4. Copy the **"Bot User OAuth Token"** — it starts with `xoxb-`. You'll need this for the `SLACK_BOT_TOKEN` env var.

## 4. Channel Setup (Automatic)

The app automatically creates the Slack channel on startup if it doesn't exist. By default, it creates **`#openspaces-discussions-test`**.

You can customize the channel name with the `SLACK_CHANNEL_NAME` env var (see step 6).

When the app creates the channel, the bot is automatically a member. If you create the channel manually or use a pre-existing one, invite the bot:

```
/invite @OpenSpaces
```

(Use whatever name you gave the app in step 1.)

## 5. Lock Down Channel Posting (Optional but Recommended)

This ensures only the bot posts top-level messages and humans can only reply in threads:

1. Open the channel settings (click the channel name at the top)
2. Go to **"Settings"** tab
3. Under **"Posting permissions"**, set to **"Admins and specific people only"** (or "Channel managers only")
4. The bot app can still post because it uses the API, but regular users will only be able to reply to existing threads

## 6. Set Environment Variables

Set these env vars wherever you run the server (Heroku, local `.env`, etc.):

```bash
SLACK_BOT_TOKEN=xoxb-your-token-here
SLACK_CHANNEL_NAME=openspaces-discussions-test
APP_BASE_URL=https://your-app.herokuapp.com
```

- `SLACK_BOT_TOKEN` — the `xoxb-` token from step 3 **(required)**
- `SLACK_CHANNEL_NAME` — the channel name to use (default: `openspaces-discussions-test`). The app finds this channel or creates it on startup.
- `APP_BASE_URL` — your app's public URL (no trailing slash). This is used in the "View in OpenSpaces" link in Slack messages. **(required)**

If `SLACK_BOT_TOKEN` or `APP_BASE_URL` are missing, the integration silently disables itself — the app works normally, just without Slack posts.

## 7. Verify It Works

1. Start/restart the server — you should see: `Slack integration enabled for channel #openspaces-discussions-test (C01ABCDEF23)`
2. Create a new topic in OpenSpaces
3. Within a few seconds, a message should appear in the Slack channel with the topic name, facilitator avatar, and a "View in OpenSpaces" link
4. The topic card in the app should show a small Slack icon linking to that thread
