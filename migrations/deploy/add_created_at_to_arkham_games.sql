-- Deploy arkham-horror-backend:add_created_at_to_arkham_games to pg
-- requires: arkham_games

BEGIN;

ALTER TABLE arkham_games
ADD COLUMN IF NOT EXISTS created_at TIMESTAMP DEFAULT now(),
ADD COLUMN IF NOT EXISTS updated_at TIMESTAMP DEFAULT now();

COMMIT;
