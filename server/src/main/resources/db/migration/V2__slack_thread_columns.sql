-- Add Slack thread tracking columns to discussions table.
-- Nullable because Slack integration is optional and because
-- the Slack post happens asynchronously after topic creation.
ALTER TABLE discussions
    ADD COLUMN slack_channel_id TEXT,
    ADD COLUMN slack_thread_ts TEXT,
    ADD COLUMN slack_permalink TEXT;
