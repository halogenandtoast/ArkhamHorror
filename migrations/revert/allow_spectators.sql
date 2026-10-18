-- Revert arkham-horror-backend:allow_spectators from pg

BEGIN;

DROP INDEX IF EXISTS arkham_games_allow_spectators_updated_at_idx;

ALTER TABLE arkham_games DROP COLUMN IF EXISTS allow_spectators;

ALTER TABLE users DROP COLUMN IF EXISTS allow_spectators_by_default;

COMMIT;
