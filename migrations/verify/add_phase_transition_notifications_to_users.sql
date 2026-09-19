-- Verify arkham-horror-backend:add_phase_transition_notifications_to_users on pg

BEGIN;

SELECT phase_transition_notifications FROM users WHERE false;

ROLLBACK;
