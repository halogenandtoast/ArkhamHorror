-- Verify arkham-horror-backend:arkham_games on pg

BEGIN;

SELECT id, current_data
  FROM arkham_games
 WHERE FALSE;

ROLLBACK;
