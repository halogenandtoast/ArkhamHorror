-- Revert arkham-horror-backend:add_payload_to_log_entries from pg

BEGIN;

DROP INDEX IF EXISTS idx_arkham_log_entry_gameid_seq;
ALTER TABLE arkham_log_entries DROP COLUMN IF EXISTS seq;
ALTER TABLE arkham_log_entries DROP COLUMN IF EXISTS payload;

COMMIT;
