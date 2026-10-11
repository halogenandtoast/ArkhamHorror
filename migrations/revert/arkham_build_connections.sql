-- Revert arkham-horror-backend:arkham_build_connections from pg

BEGIN;

DROP INDEX IF EXISTS idx_arkham_build_auth_requests_created;

DROP TABLE IF EXISTS arkham_build_auth_requests;

DROP TABLE IF EXISTS arkham_build_connections;

COMMIT;
