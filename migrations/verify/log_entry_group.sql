-- Verify arkham-horror-backend:log_entry_group on pg

BEGIN;

SELECT group_id FROM arkham_log_entries WHERE FALSE;

ROLLBACK;
