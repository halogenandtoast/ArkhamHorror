-- Deploy arkham-horror-backend:arkham_ml_decisions to pg
-- requires: arkham_games

BEGIN;

-- Live imitation-learning capture: one row per HUMAN, multi-choice decision,
-- written by the API as the decision happens (drift-free). `rows` is a
-- denormalized jsonb array, one element per flattened choice:
--   [ { "chosen": bool, "features": {67}, "breakdown": {10} } ]
-- The arkham-ml-export tool unpacks it into JSONL for ml/train.py.

CREATE TABLE IF NOT EXISTS arkham_ml_decisions (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  arkham_game_id uuid REFERENCES arkham_games (id) ON DELETE CASCADE NOT NULL,
  step integer NOT NULL,
  player_id text NOT NULL,
  chosen_index integer NOT NULL,
  rows jsonb NOT NULL,
  created_at timestamptz NOT NULL
);

CREATE INDEX IF NOT EXISTS arkham_ml_decisions_game_idx ON arkham_ml_decisions (arkham_game_id);
CREATE INDEX IF NOT EXISTS arkham_ml_decisions_created_idx ON arkham_ml_decisions (created_at);

COMMIT;
