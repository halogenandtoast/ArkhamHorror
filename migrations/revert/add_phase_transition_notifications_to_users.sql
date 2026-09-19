-- Revert arkham-horror-backend:add_phase_transition_notifications_to_users from pg

BEGIN;

ALTER TABLE users DROP COLUMN IF EXISTS phase_transition_notifications;

COMMIT;
