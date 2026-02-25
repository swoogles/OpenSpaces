-- Activities proposed by attendees (not room-bound)

CREATE TABLE activities (
    id BIGINT PRIMARY KEY,
    description TEXT NOT NULL,
    creator TEXT NOT NULL REFERENCES users(github_username) ON DELETE CASCADE,
    event_time TIMESTAMP NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    deleted_at TIMESTAMPTZ,
    slack_channel_id TEXT,
    slack_thread_ts TEXT,
    slack_permalink TEXT
);

CREATE INDEX idx_activities_event_time ON activities(event_time);
CREATE INDEX idx_activities_creator ON activities(creator);
CREATE INDEX idx_activities_deleted_at ON activities(deleted_at);

CREATE TABLE activity_interest (
    activity_id BIGINT NOT NULL REFERENCES activities(id) ON DELETE CASCADE,
    github_username TEXT NOT NULL REFERENCES users(github_username) ON DELETE CASCADE,
    interested_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (activity_id, github_username)
);

CREATE INDEX idx_activity_interest_user ON activity_interest(github_username);

