-- Deploy arkham-horror-backend:add_primary_key_to_arkham_steps to pg
-- requires: arkham_steps

BEGIN;

-- arkham_steps.id has no primary key or index, causing full sequential scans
-- (7.6M rows) on DELETE by id. Add the primary key to fix this.
--
-- CONCURRENTLY is not allowed inside a transaction block, so we commit first,
-- build the index online, then add the constraint.

COMMIT;

CREATE UNIQUE INDEX CONCURRENTLY arkham_steps_pkey_idx ON arkham_steps (id);

BEGIN;

ALTER TABLE arkham_steps ADD PRIMARY KEY USING INDEX arkham_steps_pkey_idx;

COMMIT;
