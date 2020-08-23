-- Verify arkham-horror-backend:arkham_pending_games on pg

BEGIN;

SELECT id, game_setup
  FROM arkham_pending_games
 WHERE FALSE;

ROLLBACK;
