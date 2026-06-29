-- Verify arkham-horror-backend:arkham_game_undo_floors on pg

BEGIN;

SELECT id, arkham_game_id, floor_step
  FROM arkham_game_undo_floors
 WHERE FALSE;

ROLLBACK;
