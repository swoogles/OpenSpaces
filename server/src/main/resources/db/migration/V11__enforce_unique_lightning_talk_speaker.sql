-- Reset legacy lightning talk data and prevent duplicate speakers entirely.
TRUNCATE TABLE lightning_talks;

-- Replace the active-only uniqueness with full-table uniqueness.
DROP INDEX IF EXISTS uq_lightning_talks_active_speaker;
CREATE UNIQUE INDEX uq_lightning_talks_speaker ON lightning_talks(speaker);
