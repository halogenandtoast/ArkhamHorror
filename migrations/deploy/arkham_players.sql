-- Deploy arkham-horror-backend:arkham_players to pg

BEGIN;

CREATE TABLE arkham_players (
  id bigserial primary key,
  arkham_game_id bigserial REFERENCES arkham_games (id) NOT NULL,
  user_id bigserial REFERENCES users (id) NOT NULL
);

COMMIT;
