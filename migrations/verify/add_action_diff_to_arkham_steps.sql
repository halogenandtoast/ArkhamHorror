-- Verify arkham-horror-backend:add_action_diff_to_arkham_steps on pg

BEGIN;

SELECT action_diff FROM arkham_steps WHERE false;

ROLLBACK;
