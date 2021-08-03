-- Deploy arkham-horror-backend:arkham_games to pg

BEGIN;

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE arkham_games (
  id uuid DEFAULT uuid_generate_v4(),
  name text NOT NULL,
  current_data jsonb NOT NULL,
  queue jsonb NOT NULL,
  log jsonb NOT NULL,
  multiplayerVariant text NOT NULL,
  PRIMARY KEY (id)
);

COMMIT;
