-- Add Slack user ID to users table for identity linking
ALTER TABLE users ADD COLUMN slack_user_id TEXT;

-- Index for reverse lookups (Slack ID -> GitHub username)
CREATE INDEX idx_users_slack_user_id ON users(slack_user_id) WHERE slack_user_id IS NOT NULL;
