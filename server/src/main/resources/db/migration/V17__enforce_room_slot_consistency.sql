-- V17__enforce_room_slot_consistency.sql
-- Ensure a discussion's time_slot_id always belongs to its room_id

-- Needed so (time_slot_id, room_id) can be a foreign key target.
ALTER TABLE time_slots
    ADD CONSTRAINT time_slots_id_room_unique UNIQUE (id, room_id);

-- Clean up any pre-existing mismatches before adding the constraint.
UPDATE discussions d
SET room_id = NULL,
    time_slot_id = NULL
WHERE d.time_slot_id IS NOT NULL
  AND NOT EXISTS (
      SELECT 1
      FROM time_slots ts
      WHERE ts.id = d.time_slot_id
        AND ts.room_id = d.room_id
  );

ALTER TABLE discussions
    ADD CONSTRAINT discussions_time_slot_matches_room_fk
    FOREIGN KEY (time_slot_id, room_id)
    REFERENCES time_slots (id, room_id)
    ON DELETE SET NULL;
