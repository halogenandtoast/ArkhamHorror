-- Revert arkham-horror-backend:add_features_to_users from pg

BEGIN;

ALTER TABLE users DROP COLUMN IF EXISTS features;

COMMIT;
