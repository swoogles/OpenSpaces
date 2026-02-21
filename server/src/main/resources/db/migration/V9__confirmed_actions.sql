-- Event log for all confirmed actions broadcast over WebSocket.
-- Used for visualization, analytics, and potential state replay.

CREATE TABLE confirmed_actions (
    id BIGSERIAL PRIMARY KEY,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    entity_type TEXT NOT NULL,        -- 'Discussion', 'LightningTalk', 'HackathonProject'
    action_type TEXT NOT NULL,        -- e.g., 'Vote', 'AddResult', 'Created', 'Joined'
    payload JSONB NOT NULL            -- full JSON as sent over the wire
);

CREATE INDEX idx_confirmed_actions_entity_type ON confirmed_actions(entity_type);
CREATE INDEX idx_confirmed_actions_action_type ON confirmed_actions(action_type);
CREATE INDEX idx_confirmed_actions_created_at ON confirmed_actions(created_at);
