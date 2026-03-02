-- V21__remove_invalid_wednesday_9am_slot.sql
-- Remove the invalid Wednesday Mar 4, 2026 9:00 AM slot from all rooms.
-- Any discussions assigned there must remain in the system, but become
-- unscheduled so the auto-scheduler can place them again later.

WITH invalid_slots AS (
    SELECT id
    FROM time_slots
    WHERE start_time = TIMESTAMP '2026-03-04 09:00:00'
),
unscheduled_discussions AS (
    UPDATE discussions
    SET room_id = NULL,
        time_slot_id = NULL,
        is_locked_timeslot = FALSE
    WHERE time_slot_id IN (SELECT id FROM invalid_slots)
    RETURNING id
)
DELETE FROM time_slots
WHERE id IN (SELECT id FROM invalid_slots);
