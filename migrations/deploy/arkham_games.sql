-- Deploy arkham-horror-backend:arkham_games to pg

BEGIN;

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE IF NOT EXISTS arkham_games (
  id uuid DEFAULT uuid_generate_v4(),
  name text NOT NULL,
  current_data jsonb NOT NULL,
  choices jsonb NOT NULL,
  log jsonb NOT NULL,
  multiplayer_variant text NOT NULL,
  PRIMARY KEY (id)
);

COMMIT;
