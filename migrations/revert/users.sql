-- Revert arkham-horror-backend:users from pg

BEGIN;

DROP TABLE users;

COMMIT;
