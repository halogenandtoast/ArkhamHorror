-- Revert arkham-horror-backend:arkham_game_undo_floors from pg

BEGIN;

DROP TABLE IF EXISTS arkham_game_undo_floors;

COMMIT;
