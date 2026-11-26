-- Verify arkham-horror-backend:add_primary_key_to_arkham_steps on pg

BEGIN;

SELECT 1 / (CASE WHEN EXISTS (
  SELECT 1 FROM pg_constraint
  WHERE conrelid = 'arkham_steps'::regclass AND contype = 'p'
) THEN 1 ELSE 0 END);

ROLLBACK;
