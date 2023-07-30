-- Deploy arkham-horror-backend:add_action_diff_to_arkham_steps to pg
-- requires: arkham_steps

BEGIN;

ALTER TABLE arkham_steps ADD COLUMN action_diff jsonb NOT NULL;

COMMIT;
