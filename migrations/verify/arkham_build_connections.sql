-- Verify arkham-horror-backend:arkham_build_connections on pg

BEGIN;

SELECT id, user_id, account_id, username, scopes, access_token,
       access_token_expires_at, refresh_token, refresh_token_expires_at,
       created_at, updated_at
  FROM arkham_build_connections
 WHERE FALSE;

SELECT id, user_id, redirect_uri, scopes, return_to, created_at
  FROM arkham_build_auth_requests
 WHERE FALSE;

ROLLBACK;
