-- Deploy arkham-horror-backend:akrham_decks to pg

BEGIN;

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE IF NOT EXISTS arkham_decks (
  id uuid DEFAULT uuid_generate_v4(),
  user_id bigserial REFERENCES users (id) NOT NULL,
  name text NOT NULL,
  investigator_name text NOT NULL,
  list jsonb NOT NULL,
  PRIMARY KEY (id)
);

COMMIT;
