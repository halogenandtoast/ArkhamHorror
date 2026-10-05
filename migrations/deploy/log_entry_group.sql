-- Deploy arkham-horror-backend:log_entry_group to pg
-- requires: add_key_to_log_entries

BEGIN;

-- `key` was a stable identity for a row that got rewritten in place. That is
-- gone: the log is append-only again, and a block (a skill test) is instead a
-- RUN of entries that all carry the same group id and are grouped by the
-- renderer. Many rows share one id, so the unique index has to go with it.
DROP INDEX IF EXISTS idx_arkham_log_entry_gameid_key;

ALTER TABLE arkham_log_entries RENAME COLUMN key TO group_id;

-- No index: grouping happens client-side over a 40-row tail, and undo still
-- deletes by (game, step), which is already indexed.

COMMIT;
