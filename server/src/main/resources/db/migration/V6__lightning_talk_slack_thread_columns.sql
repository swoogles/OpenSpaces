-- Add Slack thread tracking columns to lightning talks.
-- Nullable because Slack integration is optional and because
-- the Slack post happens asynchronously after proposal creation.
ALTER TABLE lightning_talks
    ADD COLUMN slack_channel_id TEXT,
    ADD COLUMN slack_thread_ts TEXT,
    ADD COLUMN slack_permalink TEXT;
