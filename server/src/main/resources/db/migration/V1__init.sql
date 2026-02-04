-- V1__init.sql
-- Event-sourced schema for StickyIcky

-- Users: GitHub identity
CREATE TABLE users (
    github_username TEXT PRIMARY KEY,
    display_name TEXT,  -- real name from GitHub profile if available
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Event log: source of truth for event sourcing
-- Every action is recorded here for audit/replay
CREATE TABLE discussion_events (
    id BIGSERIAL PRIMARY KEY,
    event_type TEXT NOT NULL,        -- Add, Delete, Vote, RemoveVote, Rename, UpdateRoomSlot, Unschedule, MoveTopic, SwapTopics
    topic_id BIGINT NOT NULL,        -- which discussion this affects
    payload JSONB NOT NULL,          -- full event data for replay
    actor TEXT NOT NULL REFERENCES users(github_username),
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_events_topic ON discussion_events(topic_id);
CREATE INDEX idx_events_actor ON discussion_events(actor);
CREATE INDEX idx_events_created ON discussion_events(created_at);
CREATE INDEX idx_events_type ON discussion_events(event_type);

-- Discussions: materialized current state (derived from events)
-- Can be rebuilt by replaying all events
CREATE TABLE discussions (
    id BIGINT PRIMARY KEY,                              -- TopicId
    topic TEXT NOT NULL,
    facilitator TEXT NOT NULL REFERENCES users(github_username),
    glyphicon TEXT NOT NULL,                            -- randomly assigned, persists with topic
    room_slot JSONB,                                    -- {room: {id, name, capacity}, timeSlot: {s, startTime, endTime}}
    interested_parties JSONB NOT NULL DEFAULT '[]',    -- [{voter: "username", position: "Interested"|"NotInterested"}, ...]
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    deleted_at TIMESTAMPTZ                              -- soft delete for audit trail
);

CREATE INDEX idx_discussions_facilitator ON discussions(facilitator);
CREATE INDEX idx_discussions_deleted ON discussions(deleted_at) WHERE deleted_at IS NULL;

-- Trigger to update updated_at on discussions
CREATE OR REPLACE FUNCTION update_updated_at()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER discussions_updated_at
    BEFORE UPDATE ON discussions
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at();
