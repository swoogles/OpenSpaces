-- Add authorization column to users table
-- Only 'swoogles' is auto-approved; everyone else starts as not approved

ALTER TABLE users ADD COLUMN approved BOOLEAN NOT NULL DEFAULT false;

-- Auto-approve the admin user
UPDATE users SET approved = true WHERE github_username = 'swoogles';

-- Add index for querying pending users
CREATE INDEX idx_users_approved ON users(approved);
