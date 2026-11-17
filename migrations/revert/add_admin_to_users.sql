-- Revert arkham-horror-backend:add_admin_to_users from pg

BEGIN;

ALTER TABLE users DROP COLUMN IF EXISTS admin;

COMMIT;
