-- Track roster message ts for each entity type's thread
-- One roster message per thread that gets edited as members change
CREATE TABLE slack_roster_messages (
    entity_type TEXT NOT NULL,  -- 'discussion', 'hackathon_project', 'activity'
    entity_id BIGINT NOT NULL,
    slack_channel_id TEXT NOT NULL,
    slack_thread_ts TEXT NOT NULL,
    roster_message_ts TEXT NOT NULL,  -- The ts of the editable roster reply
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (entity_type, entity_id)
);
