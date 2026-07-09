-- Verify arkham-horror-backend:arkham_achievements on pg

BEGIN;

SELECT id, user_id, achievement, earned_at, arkham_game_id, progress
  FROM arkham_achievements
 WHERE FALSE;

ROLLBACK;
