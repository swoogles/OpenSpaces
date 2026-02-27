-- V20__move_slots_to_next_week.sql
-- Drop this week's slots (Feb 23-27, 2026) and recreate them for next week (Mar 2-6, 2026)
-- Exceptions:
--   - No 9:00 AM slot on Monday (Mar 2, 2026)
--   - No 11:40 AM slot on Friday (Mar 6, 2026)

WITH week_slots AS (
    SELECT id, room_id, start_time, end_time
    FROM time_slots
    WHERE start_time >= TIMESTAMP '2026-02-23 00:00:00'
      AND start_time <  TIMESTAMP '2026-02-28 00:00:00'
),
unassigned_discussions AS (
    UPDATE discussions
    SET room_id = NULL,
        time_slot_id = NULL
    WHERE time_slot_id IN (SELECT id FROM week_slots)
    RETURNING id
),
removed_slots AS (
    DELETE FROM time_slots ts
    USING week_slots ws
    WHERE ts.id = ws.id
    RETURNING ws.room_id, ws.start_time, ws.end_time
)
INSERT INTO time_slots (room_id, start_time, end_time)
SELECT
    room_id,
    start_time + INTERVAL '7 days',
    end_time + INTERVAL '7 days'
FROM removed_slots
WHERE NOT (
    (start_time + INTERVAL '7 days')::date = DATE '2026-03-02'
    AND (start_time + INTERVAL '7 days')::time = TIME '09:00:00'
)
AND NOT (
    (start_time + INTERVAL '7 days')::date = DATE '2026-03-06'
    AND (start_time + INTERVAL '7 days')::time = TIME '11:40:00'
);
