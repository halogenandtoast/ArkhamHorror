-- Deploy arkham-horror-backend:arkham_steps to pg
-- requires: arkham_games

BEGIN;

ALTER TABLE arkham_games DROP COLUMN IF EXISTS choices;
ALTER TABLE arkham_games ADD COLUMN IF NOT EXISTS step INTEGER;

CREATE TABLE IF NOT EXISTS arkham_steps (
  id uuid DEFAULT uuid_generate_v4(),
  arkham_game_id uuid REFERENCES arkham_games (id) NOT NULL,
  choice jsonb NOT NULL,
  step INTEGER NOT NULL
);

CREATE UNIQUE INDEX steps_game_step_idx ON arkham_steps(arkham_game_id, step);

COMMIT;
