-- Deploy arkham-horror-backend:arkham_game_stats to pg
-- requires: arkham_games

BEGIN;

-- Pre-extracted game facts for the admin stats panel.
--
-- Everything worth counting about a game -- which campaign, which scenario, how
-- many players, how it ended, which scenarios were finished -- lives inside
-- `arkham_games.current_data`, a jsonb blob averaging 28kB and reaching 213kB.
-- There is no way to aggregate over it cheaply: a single GROUP BY on one
-- extracted field costs ~210ms over 2,200 games locally, and the panel wants a
-- dozen such aggregates. Deserializing the blob in Haskell is far worse again --
-- `fromJSON @Game` reconstructs the entire engine state per row.
--
-- So the extraction happens once, here, and the panel reads narrow indexed
-- columns. A materialized view rather than a table maintained by triggers or by
-- `updateGame`: that function is the hottest path in the application and is not
-- worth risking for a statistics page, and stats a few hours stale are still
-- the same stats. `arkham_stats_refreshes` records when it was last rebuilt so
-- the panel can say how old the numbers are.
--
-- The leading `c` is stripped from every card code. `ToJSON CardCode` prepends
-- it (`Arkham.Card.CardCode`), so the stored form is `c01104` while the engine
-- -- `allScenarioCards`, `getSideStoryCost` -- knows it as `01104`. Storing the
-- engine's form means the handler can look names up without re-deriving the
-- convention.
--
-- `gameMode` is a `These Campaign Scenario`, encoded with a key per side: a
-- campaign between scenarios has only `This`, a standalone only `That`, and a
-- campaign currently in a scenario has both. So "is a campaign" is the presence
-- of `This`, and `That` is the scenario being played right now, if any.

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

-- When each derived dataset was last rebuilt, so a panel reading one can say how
-- stale it is rather than presenting old numbers as current.
CREATE TABLE IF NOT EXISTS arkham_stats_refreshes (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  name varchar NOT NULL,
  refreshed_at timestamptz NOT NULL,
  duration_ms int NOT NULL,
  CONSTRAINT unique_stats_refresh_name UNIQUE (name)
);

-- The achievement panel counts earned rows per achievement; without this it is a
-- sequential scan of the whole table per load.
CREATE INDEX IF NOT EXISTS idx_arkham_achievements_earned
  ON arkham_achievements (achievement) WHERE earned_at IS NOT NULL;

COMMIT;
