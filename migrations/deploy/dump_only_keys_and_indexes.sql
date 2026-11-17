-- Deploy arkham-horror-backend:dump_only_keys_and_indexes to pg
-- requires: arkham_steps
-- requires: arkham_decks

BEGIN;

-- Three objects that existed only in the hand-dumped setup.sql and were never
-- in the plan, so a database built from migrations alone went without them.
-- All three are already in production; each statement is guarded or idempotent
-- so applying this there changes nothing.

-- arkham_steps.id is the primary key in the model (`Id UUID`) but the table was
-- created without one. Guarded by existence rather than applied blindly:
-- arkham_steps is the largest table in the database, and building this index
-- where it is already present would be an expensive way to do nothing.
DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conrelid = 'arkham_steps'::regclass AND contype = 'p'
  ) THEN
    EXECUTE 'ALTER TABLE arkham_steps ADD CONSTRAINT arkham_steps_pkey PRIMARY KEY (id)';
  END IF;
END
$$;

-- ArkhamDeck.userId is `UserId OnDeleteCascade`, so deleting an account has to
-- take its decks with it. add_cascades did this for players, steps and log
-- entries but not for decks.
ALTER TABLE arkham_decks
  DROP CONSTRAINT IF EXISTS arkham_decks_user_id_fkey;

ALTER TABLE arkham_decks
  ADD CONSTRAINT arkham_decks_user_id_fkey
    FOREIGN KEY (user_id)
    REFERENCES users (id)
    ON DELETE CASCADE;

-- Every deck lookup is by owner.
CREATE INDEX IF NOT EXISTS arkham_decks_user_id_idx
  ON arkham_decks (user_id);

COMMIT;
