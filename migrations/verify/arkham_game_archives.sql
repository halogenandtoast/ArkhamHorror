-- Verify arkham-horror-backend:arkham_game_archives on pg

BEGIN;

SELECT id, arkham_game_id, state, bucket, object_key, steps, top_step, bytes,
       archived_at, game_updated_at, requested_at, requested_by, restored_at,
       error_message
  FROM arkham_game_archives
 WHERE FALSE;

ROLLBACK;
