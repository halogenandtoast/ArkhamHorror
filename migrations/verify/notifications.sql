-- Verify arkham-horror-backend:notifications on pg

BEGIN;

SELECT id, body, created_at FROM notifications WHERE false;

ROLLBACK;
