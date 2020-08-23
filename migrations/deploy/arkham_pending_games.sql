-- Deploy arkham-horror-backend:arkham_pending_games to pg

BEGIN;

CREATE TABLE arkham_pending_games (
  id bigserial primary key,
  game_setup jsonb NOT NULL
);

COMMIT;
