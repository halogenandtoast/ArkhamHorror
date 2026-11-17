-- Revert arkham-horror-backend:notifications from pg

BEGIN;

DROP TABLE IF EXISTS notifications;

COMMIT;
