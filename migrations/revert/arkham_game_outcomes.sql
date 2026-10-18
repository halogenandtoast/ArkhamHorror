-- Revert arkham-horror-backend:arkham_game_outcomes from pg

BEGIN;

-- Put `arkham_game_stats` back to its shape before the outcome columns. A
-- materialized view's columns cannot be dropped in place, so it is rebuilt from
-- the definition the `arkham_game_stats` migration created, and comes back
-- `WITH NO DATA` for the same reason it shipped that way.

DROP INDEX IF EXISTS idx_arkham_game_stats_outcome;

DROP TABLE IF EXISTS arkham_game_outcomes;

DROP MATERIALIZED VIEW IF EXISTS arkham_game_stats;

CREATE MATERIALIZED VIEW arkham_game_stats AS
SELECT
  g.id                                                    AS game_id,
  g.created_at,
  g.updated_at,
  g.multiplayer_variant,
  (g.current_data -> 'gameMode' ? 'This')                 AS is_campaign,
  g.current_data #>> '{gameMode,This,id}'                 AS campaign_id,
  CASE
    WHEN g.current_data #>> '{gameMode,That,id}' LIKE 'c%'
      THEN substr(g.current_data #>> '{gameMode,That,id}', 2)
    ELSE g.current_data #>> '{gameMode,That,id}'
  END                                                     AS scenario_id,
  -- One difficulty per game; the scenario copies the campaign's when both exist.
  COALESCE(
    g.current_data #>> '{gameMode,This,difficulty}',
    g.current_data #>> '{gameMode,That,difficulty}'
  )                                                       AS difficulty,
  g.current_data #>> '{gameGameState,tag}'                AS state,
  CASE
    WHEN jsonb_typeof(g.current_data -> 'gamePlayerCount') = 'number'
      THEN (g.current_data ->> 'gamePlayerCount')::int
  END                                                     AS player_count,
  COALESCE(r.completed, '{}'::text[])                     AS completed_scenarios,
  COALESCE(r.resolved, '{}'::text[])                      AS resolved_scenarios
FROM arkham_games g
-- Lateral rather than a grouped CTE joined back on id: both forms cost the same
-- per row, but this one touches `current_data` in a single pass over the table
-- instead of detoasting every blob twice.
LEFT JOIN LATERAL (
  SELECT
    -- Every scenario this campaign has finished, and how. `resolutions` is keyed
    -- by scenario code; `NoResolution` is one that was lost or resigned out of,
    -- which is the difference between "played" and "won".
    array_agg(
      CASE WHEN e.key LIKE 'c%' THEN substr(e.key, 2) ELSE e.key END ORDER BY e.key
    ) AS completed,
    array_agg(
      CASE WHEN e.key LIKE 'c%' THEN substr(e.key, 2) ELSE e.key END ORDER BY e.key
    ) FILTER (WHERE e.value ->> 'tag' = 'Resolution') AS resolved
  FROM jsonb_each(
    -- `jsonb_each` raises on a non-object, and a standalone game has no
    -- `resolutions` at all, so the path is normalised before it is expanded.
    CASE
      WHEN jsonb_typeof(g.current_data #> '{gameMode,This,resolutions}') = 'object'
        THEN g.current_data #> '{gameMode,This,resolutions}'
      ELSE '{}'::jsonb
    END
  ) AS e(key, value)
) r ON TRUE
-- Deliberately NOT populated here. Building it costs ~0.7ms per game -- the blob
-- has to be detoasted and parsed once each -- so on a large table this would hold
-- the migration, and the `migrate` service, for minutes before the app could
-- start. The panel reads `pg_matviews.ispopulated` and offers to build it.
WITH NO DATA;

-- Unique on the key, which is what lets a later refresh run CONCURRENTLY if the
-- rebuild ever grows long enough to be worth not blocking readers for.
CREATE UNIQUE INDEX IF NOT EXISTS idx_arkham_game_stats_game
  ON arkham_game_stats (game_id);

CREATE INDEX IF NOT EXISTS idx_arkham_game_stats_campaign
  ON arkham_game_stats (campaign_id) WHERE is_campaign;

CREATE INDEX IF NOT EXISTS idx_arkham_game_stats_scenario
  ON arkham_game_stats (scenario_id) WHERE NOT is_campaign;

CREATE INDEX IF NOT EXISTS idx_arkham_game_stats_created
  ON arkham_game_stats (created_at);

COMMIT;
