-- Deploy arkham-horror-backend:arkham_achievements to pg
-- requires: arkham_games
-- requires: users

BEGIN;

-- Above-the-table achievements: one row per user per achievement, forever.
-- arkham_game_id links back to the earning game and degrades to NULL when
-- that game is deleted. progress carries counter/checklist state for
-- cross-playthrough achievements while earned_at is still NULL.

CREATE TABLE IF NOT EXISTS arkham_achievements (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  user_id bigint REFERENCES users (id) ON DELETE CASCADE NOT NULL,
  achievement varchar NOT NULL,
  earned_at timestamptz,
  arkham_game_id uuid REFERENCES arkham_games (id) ON DELETE SET NULL,
  progress jsonb NOT NULL,
  CONSTRAINT unique_user_achievement UNIQUE (user_id, achievement)
);

-- The campaign log tab queries by earning game.
CREATE INDEX IF NOT EXISTS idx_arkham_achievements_game
  ON arkham_achievements (arkham_game_id);

COMMIT;
