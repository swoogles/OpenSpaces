-- V15__rooms_and_time_slots.sql
-- Normalize rooms and time slots into proper tables

-- Rooms table (replaces hardcoded Room.scala instances)
CREATE TABLE rooms (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    capacity INT NOT NULL
);

-- Time slots table (replaces hardcoded DiscussionState.timeSlotExamples)
-- Each row represents one slot in one room at a specific time
CREATE TABLE time_slots (
    id SERIAL PRIMARY KEY,
    room_id INT NOT NULL REFERENCES rooms(id) ON DELETE CASCADE,
    start_time TIMESTAMP NOT NULL,
    end_time TIMESTAMP NOT NULL,
    UNIQUE (room_id, start_time)
);

CREATE INDEX idx_time_slots_start ON time_slots(start_time);
CREATE INDEX idx_time_slots_room ON time_slots(room_id);

-- Add normalized FK columns to discussions (will migrate from room_slot JSONB)
ALTER TABLE discussions
    ADD COLUMN room_id INT REFERENCES rooms(id) ON DELETE SET NULL,
    ADD COLUMN time_slot_id INT REFERENCES time_slots(id) ON DELETE SET NULL;

-- Add constraint: if one is set, both must be set
ALTER TABLE discussions
    ADD CONSTRAINT discussions_room_time_slot_both_or_neither
    CHECK ((room_id IS NULL AND time_slot_id IS NULL) OR (room_id IS NOT NULL AND time_slot_id IS NOT NULL));

-- Drop the old JSONB column (we're truncating data anyway per Bill's instruction)
ALTER TABLE discussions DROP COLUMN room_slot;
