-- Deploy arkham-horror-backend:add_key_to_log_entries to pg
-- requires: add_payload_to_log_entries

BEGIN;

-- A stable identity for an entry that is written once and then revised.
--
-- A skill test's block is why this exists: it appears when the test begins and
-- is rewritten as the test proceeds, so a later send with the same key has to
-- find and update that row rather than add another. Entries attached to the
-- block (a committed card, a revealed token, the clue it discovered) are looked
-- up by the same key.
--
-- Nullable, because almost nothing needs one: an ordinary line is written once
-- and never revised.
ALTER TABLE arkham_log_entries ADD COLUMN IF NOT EXISTS key TEXT;

-- Partial: only keyed rows are ever looked up this way, and they are a tiny
-- fraction of the table. Unique, because two rows sharing a key within a game
-- is exactly the duplication the key exists to prevent.
CREATE UNIQUE INDEX IF NOT EXISTS idx_arkham_log_entry_gameid_key
  ON arkham_log_entries (arkham_game_id, key)
  WHERE key IS NOT NULL;

COMMIT;
