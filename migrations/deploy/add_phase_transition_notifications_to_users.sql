-- Deploy arkham-horror-backend:add_phase_transition_notifications_to_users to pg
-- requires: users

BEGIN;

ALTER TABLE users ADD COLUMN IF NOT EXISTS phase_transition_notifications BOOLEAN NOT NULL DEFAULT FALSE;

COMMIT;
