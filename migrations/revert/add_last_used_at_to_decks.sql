-- Revert arkham-horror-backend:add_last_used_at_to_decks from pg

BEGIN;

DROP INDEX IF EXISTS idx_arkham_decks_user_last_used;
ALTER TABLE arkham_decks DROP COLUMN IF EXISTS last_used_at;

COMMIT;
