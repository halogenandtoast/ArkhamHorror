-- Verify arkham-horror-backend:arkham_game_stats on pg

BEGIN;

SELECT game_id, created_at, updated_at, multiplayer_variant, is_campaign,
       campaign_id, scenario_id, difficulty, state, player_count,
       completed_scenarios, resolved_scenarios
  FROM arkham_game_stats
 WHERE FALSE;

SELECT id, name, refreshed_at, duration_ms
  FROM arkham_stats_refreshes
 WHERE FALSE;

ROLLBACK;
