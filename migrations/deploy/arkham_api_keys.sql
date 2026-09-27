-- Deploy arkham-horror-backend:arkham_api_keys to pg
-- requires: users

BEGIN;

-- Per-user credentials for programmatic access, so an agent can be given the
-- ability to write custom cards without being given the account.
--
-- The account token cannot do this job: `jsonToToken` sets only `iss` and `iat`,
-- so a login token never expires, covers everything the user can do (decks,
-- games, DELETE /account), and cannot be revoked except by rotating the global
-- signing secret -- which signs out every user at once. A key is scoped,
-- revocable on its own, and shows when it was last used.
--
-- Only the digest is stored. The key is high-entropy random bytes rather than a
-- password, so a single SHA-256 is the right hash: bcrypt exists to make
-- guessing a low-entropy secret slow, and there is nothing to guess here -- while
-- paying bcrypt on every API call would be a self-inflicted rate limit.
--
-- `prefix` is the leading, non-secret part of the key. It is what the UI lists
-- and what a log line can name, so a user with four keys can tell which one to
-- revoke without either of us ever holding the whole thing again.
--
-- `scopes` is a space-separated list in the manner of an OAuth scope string
-- (`cards:read cards:write`), so the column does not have to change if these
-- keys are later issued by an authorization server.
--
-- `usage_window_start`/`usage_count` are a fixed-window rate limiter kept on the
-- row itself. In the app it would have to be per-replica, and a limit that a
-- second replica doubles is not a limit; here it also survives someone calling
-- the API directly instead of going through the MCP server.

CREATE TABLE IF NOT EXISTS arkham_api_keys (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  user_id bigint REFERENCES users (id) ON DELETE CASCADE NOT NULL,
  name varchar NOT NULL,
  prefix varchar NOT NULL,
  digest varchar NOT NULL,
  scopes varchar NOT NULL DEFAULT '',
  last_used_at timestamptz,
  expires_at timestamptz,
  revoked_at timestamptz,
  usage_window_start timestamptz,
  usage_count int NOT NULL DEFAULT 0,
  created_at timestamptz NOT NULL,
  CONSTRAINT unique_api_key_digest UNIQUE (digest)
);

-- Every authenticated request presenting a key looks it up by digest, which the
-- unique constraint already indexes. This one is for the user's own list.
CREATE INDEX IF NOT EXISTS idx_arkham_api_keys_user
  ON arkham_api_keys (user_id);

COMMIT;
