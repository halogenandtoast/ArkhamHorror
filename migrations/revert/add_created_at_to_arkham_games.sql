-- Revert arkham-horror-backend:add_created_at_to_arkham_games from pg

BEGIN;

ALTER TABLE arkham_games
DROP COLUMN created_at,
DROP COLUMN updated_at;

COMMIT;
