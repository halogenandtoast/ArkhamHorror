-- Revert arkham-horror-backend:add_key_to_log_entries from pg

BEGIN;

DROP INDEX IF EXISTS idx_arkham_log_entry_gameid_key;
ALTER TABLE arkham_log_entries DROP COLUMN IF EXISTS key;

COMMIT;
