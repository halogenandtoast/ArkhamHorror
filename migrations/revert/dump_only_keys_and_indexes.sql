-- Revert arkham-horror-backend:dump_only_keys_and_indexes from pg

BEGIN;

DROP INDEX IF EXISTS arkham_decks_user_id_idx;

ALTER TABLE arkham_decks
  DROP CONSTRAINT IF EXISTS arkham_decks_user_id_fkey;

ALTER TABLE arkham_decks
  ADD CONSTRAINT arkham_decks_user_id_fkey
    FOREIGN KEY (user_id)
    REFERENCES users (id);

ALTER TABLE arkham_steps DROP CONSTRAINT IF EXISTS arkham_steps_pkey;

COMMIT;
