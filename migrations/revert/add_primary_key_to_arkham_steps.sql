-- Revert arkham-horror-backend:add_primary_key_to_arkham_steps from pg

BEGIN;

-- Left in place deliberately: dump_only_keys_and_indexes is the entry that
-- owns this constraint now, and its own revert drops it. Dropping it here too
-- would take the primary key away from a database that is only reverting this
-- one entry.

COMMIT;
