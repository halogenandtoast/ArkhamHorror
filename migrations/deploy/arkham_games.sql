-- Deploy arkham-horror-backend:arkham_games to pg

BEGIN;

CREATE TABLE arkham_games (
  id bigserial primary key,
  current_data jsonb NOT NULL
);

COMMIT;
