-- Revert arkham-horror-backend:align_model_column_types from pg

BEGIN;

ALTER TABLE arkham_game_undo_floors
  ALTER COLUMN floor_step TYPE integer;

COMMIT;
