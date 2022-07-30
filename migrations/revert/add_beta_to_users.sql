-- Revert arkham-horror-backend:add_beta_to_users from pg

BEGIN;

ALTER TABLE users DROP COLUMN beta;

COMMIT;
