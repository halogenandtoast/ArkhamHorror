-- Verify arkham-horror-backend:arkham_api_keys on pg

BEGIN;

SELECT id, user_id, name, prefix, digest, scopes, last_used_at, expires_at,
       revoked_at, usage_window_start, usage_count, created_at
  FROM arkham_api_keys
 WHERE FALSE;

ROLLBACK;
