-- Verify arkham-horror-backend:arkham_game_outcomes on pg

BEGIN;

SELECT game_id, outcome_result, outcome_certainty, outcome_note, outcome_source, outcome_scenario,
       outcome_resolution_tag, outcome_resolution
  FROM arkham_game_stats
 WHERE FALSE;

SELECT id, arkham_game_id, result, certainty, note, source, scenario_id, resolution_tag,
       resolution, computed_at
  FROM arkham_game_outcomes
 WHERE FALSE;

ROLLBACK;
