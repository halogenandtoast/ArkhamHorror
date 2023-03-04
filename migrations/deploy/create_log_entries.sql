-- Deploy arkham-horror-backend:create_log_entries to pg
-- requires: arkham_games

BEGIN;

ALTER TABLE arkham_games DROP COLUMN IF EXISTS log;

CREATE TABLE IF NOT EXISTS arkham_log_entries (
  id bigserial primary key,
  body text NOT NULL,
  arkham_game_id uuid REFERENCES arkham_games (id) NOT NULL,
  step INTEGER NOT NULL,
  created_at TIMESTAMP DEFAULT now()
);

CREATE INDEX log_entries_game_id ON arkham_log_entries(arkham_game_id);

COMMIT;
