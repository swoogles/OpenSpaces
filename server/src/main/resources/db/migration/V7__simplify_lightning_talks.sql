-- Lightning talks are now a simple participation toggle (no freeform title).
-- Remove legacy columns that tracked proposal text and auto-updated timestamps.
DROP TRIGGER IF EXISTS lightning_talks_updated_at ON lightning_talks;

ALTER TABLE lightning_talks
    DROP COLUMN IF EXISTS topic,
    DROP COLUMN IF EXISTS updated_at;
