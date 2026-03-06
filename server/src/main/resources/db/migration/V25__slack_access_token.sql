-- Store Slack access token for posting on behalf of users
-- This enables auto-following threads by posting a wave emoji when users vote
ALTER TABLE users ADD COLUMN slack_access_token TEXT;
