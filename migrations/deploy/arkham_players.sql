-- Deploy arkham-horror-backend:arkham_players to pg

BEGIN;

CREATE TABLE IF NOT EXISTS arkham_players (
  id bigserial primary key,
  arkham_game_id uuid REFERENCES arkham_games (id) NOT NULL,
  user_id bigserial REFERENCES users (id) NOT NULL,
  investigator_id text NOT NULL
);

COMMIT;
