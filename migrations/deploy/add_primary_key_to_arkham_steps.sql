-- Deploy arkham-horror-backend:add_primary_key_to_arkham_steps to pg
-- requires: arkham_steps
-- requires: dump_only_keys_and_indexes

BEGIN;

-- arkham_steps.id is the table's identity in the model, but the table was
-- created without a primary key, so a DELETE by id sequentially scanned the
-- largest table in the database. The fix was written in April as a deploy
-- script that never made it into the plan, so the only places it ever reached
-- were a hand-run psql session and the dump taken afterwards.
--
-- dump_only_keys_and_indexes has since brought that constraint into the plan,
-- so by the time this runs it is already there and the guard skips: this entry
-- exists so the script is part of the plan rather than a loose file that would
-- build a second unique index over the whole table if anyone ever ran it.
--
-- The original used CREATE UNIQUE INDEX CONCURRENTLY to avoid holding a write
-- lock on a 7.6M-row table. That is not possible inside a guard (CONCURRENTLY
-- cannot run in a transaction or a DO block) and is no longer worth it: the
-- only way to arrive here without the constraint is to have reverted it, and
-- a reverted database is not one with live traffic on it.
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

COMMIT;
