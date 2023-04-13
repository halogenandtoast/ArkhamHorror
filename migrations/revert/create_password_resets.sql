-- Revert arkham-horror-backend:create_password_resets from pg

BEGIN;

DROP TABLE IF EXISTS password_resets;


COMMIT;
