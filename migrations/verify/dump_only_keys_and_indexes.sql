-- Verify arkham-horror-backend:dump_only_keys_and_indexes on pg

BEGIN;

SELECT 1 / (CASE WHEN EXISTS (
  SELECT 1 FROM pg_constraint
  WHERE conrelid = 'arkham_steps'::regclass AND contype = 'p'
) AND EXISTS (
  SELECT 1 FROM pg_indexes
  WHERE schemaname = 'public' AND indexname = 'arkham_decks_user_id_idx'
) THEN 1 ELSE 0 END);

ROLLBACK;
