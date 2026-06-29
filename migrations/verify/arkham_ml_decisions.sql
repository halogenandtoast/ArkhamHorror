-- Verify arkham-horror-backend:arkham_ml_decisions on pg

BEGIN;

SELECT id, arkham_game_id, step, player_id, chosen_index, rows, created_at
  FROM arkham_ml_decisions
 WHERE FALSE;

ROLLBACK;
