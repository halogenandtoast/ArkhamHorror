-- Deploy arkham-horror-backend:arkham_game_undo_floors to pg
-- requires: arkham_games

BEGIN;

-- Per-game undo floor: the lowest step a game may be undone back to. One row
-- per game (enforced by the unique constraint).
CREATE TABLE IF NOT EXISTS arkham_game_undo_floors (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  arkham_game_id uuid REFERENCES arkham_games (id) ON DELETE CASCADE NOT NULL,
  floor_step integer NOT NULL,
  UNIQUE (arkham_game_id)
);

COMMIT;
