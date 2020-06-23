-- Verify arkham-horror-backend:users on pg

BEGIN;

SELECT id, username, email, password_digest
  FROM users
 WHERE FALSE;

ROLLBACK;
