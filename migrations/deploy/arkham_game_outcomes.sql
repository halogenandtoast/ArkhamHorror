-- Deploy arkham-horror-backend:arkham_game_outcomes to pg
-- requires: arkham_game_stats

BEGIN;

-- How a finished game ended, for the admin stats panel.
--
-- A finished game used to be just `state = 'IsOver'`: won campaigns and wiped
-- parties were indistinguishable, which is why `arkham_game_stats` could count
-- `finished` but never `won`. The engine now records how a game ended in
-- `current_data -> 'gameOutcome'` (see "Arkham.Game.Outcome").
--
-- Games that finished before it did have nothing recorded, and there are a lot
-- of them. Their verdict is reconstructed instead -- on read for anything
-- showing one game, and, for the panel, once into `arkham_game_outcomes` by an
-- admin action.
--
-- That reconstruction deliberately does NOT write into `current_data`. Doing so
-- would rewrite a ~28kB TOASTed blob per game on the largest table in the
-- database, and would put state into the column the undo/replay machinery
-- patches, for a statistics page. A row in a narrow side table costs a few dozen
-- bytes and is attributable: `source` says the verdict was reconstructed, and
-- deleting the table loses nothing that cannot be worked out again.
--
-- The rule of thumb that does the reconstructing lives in Haskell
-- (`inferCampaignOutcome`), in one copy. Nothing here re-implements it.

-- One row per game whose outcome had to be worked out after the fact. Absent for
-- a game the engine recorded an outcome for -- that one is in the blob, and wins.
CREATE TABLE IF NOT EXISTS arkham_game_outcomes (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  arkham_game_id uuid NOT NULL REFERENCES arkham_games (id) ON DELETE CASCADE,
  result text NOT NULL,
  certainty text NOT NULL DEFAULT 'Certain',
  note text,
  source text NOT NULL,
  scenario_id text,
  resolution_tag text,
  resolution int,
  computed_at timestamptz NOT NULL DEFAULT now(),
  CONSTRAINT unique_game_outcome UNIQUE (arkham_game_id),
  CONSTRAINT valid_game_outcome_result CHECK (result IN ('Won', 'Lost')),
  CONSTRAINT valid_game_outcome_certainty CHECK (certainty IN ('Certain', 'Uncertain'))
);

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
  -- The engine's own record first, then the reconstructed one. 'Won' / 'Lost',
  -- or NULL for a finished game neither has an answer for yet.
  COALESCE(g.current_data #>> '{gameOutcome,result}', o.result)
                                                          AS outcome_result,
  -- 'Certain', or 'Uncertain' for an ending the Campaign Guide prints as "The
  -- investigators win the campaign...?" -- a win it will not commit to, which is
  -- worth counting apart from one stated outright.
  COALESCE(g.current_data #>> '{gameOutcome,certainty}', o.certainty, 'Certain')
                                                          AS outcome_certainty,
  -- i18n key for the clause the guide attaches to the verdict, if any.
  COALESCE(g.current_data #>> '{gameOutcome,note}', o.note)
                                                          AS outcome_note,
  -- 'Recorded' (the ending said so) / 'Inferred' (worked out afterwards).
  COALESCE(g.current_data #>> '{gameOutcome,source}', o.source)
                                                          AS outcome_source,
  -- The ending it stopped on. Same card-code convention as `scenario_id`.
  CASE
    WHEN COALESCE(g.current_data #>> '{gameOutcome,ending,scenario}', o.scenario_id) LIKE 'c%'
      THEN substr(COALESCE(g.current_data #>> '{gameOutcome,ending,scenario}', o.scenario_id), 2)
    ELSE COALESCE(g.current_data #>> '{gameOutcome,ending,scenario}', o.scenario_id)
  END                                                     AS outcome_scenario,
  -- 'NoResolution' for a scenario lost or resigned out of, else 'Resolution'
  -- with the number in `outcome_resolution`.
  COALESCE(g.current_data #>> '{gameOutcome,ending,resolution,tag}', o.resolution_tag)
                                                          AS outcome_resolution_tag,
  COALESCE(
    CASE
      WHEN jsonb_typeof(g.current_data #> '{gameOutcome,ending,resolution,contents}') = 'number'
        THEN (g.current_data #>> '{gameOutcome,ending,resolution,contents}')::int
    END,
    o.resolution
  )                                                       AS outcome_resolution,
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
LEFT JOIN arkham_game_outcomes o ON o.arkham_game_id = g.id
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

-- The backfill looks for finished games with no recorded outcome, and the panel
-- counts outcomes per campaign; both scan on these two columns.
CREATE INDEX IF NOT EXISTS idx_arkham_game_stats_outcome
  ON arkham_game_stats (outcome_result) WHERE state = 'IsOver';

COMMIT;
