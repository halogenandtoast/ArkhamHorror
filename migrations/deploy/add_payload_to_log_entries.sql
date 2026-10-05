-- Deploy arkham-horror-backend:add_payload_to_log_entries to pg
-- requires: add_urls_to_card_sets

BEGIN;

-- The structured game-log entry. Nullable: every row written before the
-- overhaul has only `body`, the flat brace-DSL string, and the client parses
-- those once at ingest rather than keeping a second renderer forever.
-- See docs/game-log/.
ALTER TABLE arkham_log_entries ADD COLUMN IF NOT EXISTS payload JSONB;

-- Scrollback pages backwards by seq within a game. `step` already has an index
-- (idx_arkham_log_entry_gameid_step) and stays load-bearing for undo, which
-- deletes entries by step; seq is the client's own cursor and ordering key.
ALTER TABLE arkham_log_entries ADD COLUMN IF NOT EXISTS seq INTEGER;

CREATE INDEX IF NOT EXISTS idx_arkham_log_entry_gameid_seq
  ON arkham_log_entries (arkham_game_id, seq DESC);

COMMIT;
