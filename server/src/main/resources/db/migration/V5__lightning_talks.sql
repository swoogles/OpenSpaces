-- Lightning talk proposals and assignments.
-- One active proposal per speaker; assignment slots are unique per night.
CREATE TABLE lightning_talks (
    id BIGINT PRIMARY KEY,
    topic TEXT NOT NULL,
    speaker TEXT NOT NULL REFERENCES users(github_username),
    assignment_night TEXT CHECK (assignment_night IN ('Monday', 'Tuesday', 'Thursday')),
    assignment_slot INT CHECK (assignment_slot BETWEEN 1 AND 10),
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    deleted_at TIMESTAMPTZ,
    CHECK (
      (assignment_night IS NULL AND assignment_slot IS NULL) OR
      (assignment_night IS NOT NULL AND assignment_slot IS NOT NULL)
    )
);

CREATE INDEX idx_lightning_talks_speaker ON lightning_talks(speaker);
CREATE INDEX idx_lightning_talks_deleted ON lightning_talks(deleted_at) WHERE deleted_at IS NULL;

-- One active proposal per speaker globally across all nights.
CREATE UNIQUE INDEX uq_lightning_talks_active_speaker
    ON lightning_talks(speaker)
    WHERE deleted_at IS NULL;

-- Prevent slot collisions for active proposals.
CREATE UNIQUE INDEX uq_lightning_talks_active_assignment
    ON lightning_talks(assignment_night, assignment_slot)
    WHERE deleted_at IS NULL AND assignment_night IS NOT NULL;

CREATE TRIGGER lightning_talks_updated_at
    BEFORE UPDATE ON lightning_talks
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at();
