-- Verify arkham-horror-backend:align_model_column_types on pg

BEGIN;

SELECT 1 / (CASE WHEN EXISTS (
  SELECT 1 FROM information_schema.columns
  WHERE table_name = 'arkham_game_undo_floors'
    AND column_name = 'floor_step'
    AND data_type = 'bigint'
) THEN 1 ELSE 0 END);

ROLLBACK;
