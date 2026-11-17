-- Deploy arkham-horror-backend:align_model_column_types to pg
-- requires: arkham_game_undo_floors

BEGIN;

-- ArkhamGameUndoFloor.floorStep is `Int`, which persistent maps to bigint, but
-- the migration that created the table said `integer`. Production already has
-- bigint (setup.sql was dumped from a database corrected by hand), so this is
-- a no-op there; a database built from the plan alone needs it.
ALTER TABLE arkham_game_undo_floors
  ALTER COLUMN floor_step TYPE bigint;

COMMIT;
