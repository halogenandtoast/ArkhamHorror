-- Revert arkham-horror-backend:add_action_diff_to_arkham_steps from pg

BEGIN;

ALTER TABLE arkham_steps DROP COLUMN action_diff;

COMMIT;
