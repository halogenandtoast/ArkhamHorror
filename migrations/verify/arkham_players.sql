-- Verify arkham-horror-backend:arkham_players on pg

BEGIN;

SELECT id, arkham_game_id, user_id
  FROM arkham_players
 WHERE FALSE;

ROLLBACK;
