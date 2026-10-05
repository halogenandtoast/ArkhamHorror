-- Verify arkham-horror-backend:add_key_to_log_entries on pg

BEGIN;

SELECT key FROM arkham_log_entries WHERE FALSE;

ROLLBACK;
