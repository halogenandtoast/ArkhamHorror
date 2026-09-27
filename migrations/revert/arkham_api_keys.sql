-- Revert arkham-horror-backend:arkham_api_keys from pg

BEGIN;

DROP INDEX IF EXISTS idx_arkham_api_keys_user;

DROP TABLE IF EXISTS arkham_api_keys;

COMMIT;
