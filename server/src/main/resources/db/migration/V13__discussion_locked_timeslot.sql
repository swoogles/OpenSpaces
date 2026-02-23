-- Allow admins to lock a topic's scheduled timeslot so auto-scheduling won't move it.
ALTER TABLE discussions
  ADD COLUMN is_locked_timeslot BOOLEAN NOT NULL DEFAULT FALSE;

