-- Deploy arkham-horror-backend:arkham_build_connections to pg
-- requires: users

BEGIN;

-- A user's link to their arkham.build account, held as OAuth tokens rather than
-- as a password. We are a confidential client: the client secret stays on this
-- server and the browser never sees either token.
--
-- Both tokens are stored in the clear. They have to be usable, so a digest is no
-- help here the way it is for arkham_api_keys, and anything short of a KMS key
-- would only move the secret to another row in the same database. The mitigation
-- that does work is scope: we ask for `profile:read decks:read`, so the worst a
-- leaked token can do is read decks the user already publishes.
--
-- `refresh_token` rotates on every use and each one works exactly once, so the
-- replacement must be written before another refresh is attempted. Callers take
-- `FOR UPDATE` on the row for the length of the exchange.
CREATE TABLE IF NOT EXISTS arkham_build_connections (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  user_id bigint REFERENCES users (id) ON DELETE CASCADE NOT NULL,
  account_id varchar NOT NULL,
  username varchar NOT NULL,
  scopes varchar NOT NULL DEFAULT '',
  access_token varchar NOT NULL,
  access_token_expires_at timestamptz NOT NULL,
  refresh_token varchar NOT NULL,
  refresh_token_expires_at timestamptz NOT NULL,
  created_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL,
  CONSTRAINT unique_arkham_build_connection_user UNIQUE (user_id)
);

-- An authorization in flight. The row's id is the OAuth `state`: a v4 UUID from
-- the system CSPRNG, handed out by an authenticated request and looked up again
-- by the callback, which arrives as a browser redirect carrying no Authorization
-- header and so has no other way to know whose connection it is completing.
--
-- `return_to` is a path on this site, never a full URL, so a callback cannot be
-- talked into bouncing the browser somewhere else.
CREATE TABLE IF NOT EXISTS arkham_build_auth_requests (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  user_id bigint REFERENCES users (id) ON DELETE CASCADE NOT NULL,
  redirect_uri varchar NOT NULL,
  scopes varchar NOT NULL,
  return_to varchar NOT NULL,
  created_at timestamptz NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_arkham_build_auth_requests_created
  ON arkham_build_auth_requests (created_at);

COMMIT;
