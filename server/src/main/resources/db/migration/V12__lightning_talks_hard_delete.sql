-- Reset legacy data and switch lightning talks to hard-delete semantics.
TRUNCATE TABLE lightning_talks;

-- Remove indexes that depend on deleted_at.
DROP INDEX IF EXISTS idx_lightning_talks_deleted;
DROP INDEX IF EXISTS uq_lightning_talks_active_assignment;

-- Recreate assignment uniqueness for scheduled talks only.
CREATE UNIQUE INDEX uq_lightning_talks_active_assignment
    ON lightning_talks(assignment_night, assignment_slot)
    WHERE assignment_night IS NOT NULL;

-- Rows are now physically deleted on opt-out.
ALTER TABLE lightning_talks
    DROP COLUMN IF EXISTS deleted_at;
