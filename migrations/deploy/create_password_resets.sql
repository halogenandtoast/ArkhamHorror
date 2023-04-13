-- Deploy arkham-horror-backend:create_password_resets to pg
-- requires: users

BEGIN;

CREATE TABLE IF NOT EXISTS password_resets (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v1mc(),
  user_id bigserial REFERENCES users NOT NULL,
  expires_at timestamp with time zone NOT NULL
);

COMMIT;
