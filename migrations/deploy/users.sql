-- Deploy arkham-horror-backend:users to pg

BEGIN;

CREATE TABLE IF NOT EXISTS users (
  id bigserial primary key,
  username character varying NOT NULL UNIQUE,
  email character varying NOT NULL UNIQUE,
  password_digest character varying NOT NULL
);

COMMIT;
