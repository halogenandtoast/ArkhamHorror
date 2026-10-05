-- Revert arkham-horror-backend:log_entry_group from pg

BEGIN;

ALTER TABLE arkham_log_entries RENAME COLUMN group_id TO key;

CREATE UNIQUE INDEX IF NOT EXISTS idx_arkham_log_entry_gameid_key
  ON arkham_log_entries (arkham_game_id, key)
  WHERE key IS NOT NULL;

COMMIT;
