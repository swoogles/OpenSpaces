-- Track users who dismissed (voted "not interested" on) activities
-- This mirrors the Topics pattern where both interested and not-interested votes are tracked

CREATE TABLE activity_dismissal (
    activity_id BIGINT NOT NULL REFERENCES activities(id) ON DELETE CASCADE,
    github_username TEXT NOT NULL REFERENCES users(github_username) ON DELETE CASCADE,
    dismissed_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (activity_id, github_username)
);

CREATE INDEX idx_activity_dismissal_user ON activity_dismissal(github_username);
