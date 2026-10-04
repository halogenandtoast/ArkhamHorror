-- Revert arkham-horror-backend:arkham_game_stats from pg

BEGIN;

DROP INDEX IF EXISTS idx_arkham_achievements_earned;

DROP TABLE IF EXISTS arkham_stats_refreshes;

DROP MATERIALIZED VIEW IF EXISTS arkham_game_stats;

COMMIT;
