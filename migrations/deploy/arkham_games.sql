-- Deploy arkham-horror-backend:arkham_games to pg

BEGIN;

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE arkham_games (
  id uuid DEFAULT uuid_generate_v4(),
  current_data jsonb NOT NULL,
  PRIMARY KEY (id)
);

COMMIT;
