-- Add actor column to confirmed_actions for easier user identification.
-- Stores the GitHub username of the user who performed the action.

ALTER TABLE confirmed_actions
    ADD COLUMN actor TEXT;

CREATE INDEX idx_confirmed_actions_actor ON confirmed_actions(actor);
