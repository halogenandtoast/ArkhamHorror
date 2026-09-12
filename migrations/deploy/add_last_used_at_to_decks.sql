-- Deploy arkham-horror-backend:add_last_used_at_to_decks to pg
-- requires: arkham_decks
-- requires: arkham_players

BEGIN;

-- When the deck was last taken into a game. Stamped where a deck is chosen for
-- a seat (and where a campaign upgrade rewrites it), so the decks page can put
-- what you are actually playing at the top. Null means never used.

ALTER TABLE arkham_decks ADD COLUMN IF NOT EXISTS last_used_at timestamptz;

-- Backfill an approximation for decks that predate the column: the newest game
-- in which this deck's owner sat as this deck's investigator. Deck choices were
-- never recorded, so a game's updated_at (last played) stands in for the moment
-- the deck was picked, and an owner with two decks for the same investigator
-- gets the same time on both. Only ever a starting order -- every use from here
-- writes the real timestamp over it.
UPDATE arkham_decks d
SET last_used_at = sub.used_at
FROM (
  -- arkham_games.updated_at predates the timestamptz convention and holds UTC
  -- wall-clock in a bare timestamp, so say so rather than leaning on the
  -- server's timezone to read it.
  SELECT p.user_id, p.investigator_id, MAX(g.updated_at) AT TIME ZONE 'UTC' AS used_at
  FROM arkham_players p
  JOIN arkham_games g ON g.id = p.arkham_game_id
  WHERE p.investigator_id <> '00000'
  GROUP BY p.user_id, p.investigator_id
) sub
WHERE d.user_id = sub.user_id
  AND d.list->>'investigator_code' = sub.investigator_id;

-- The decks page orders a single owner's decks by this.
CREATE INDEX IF NOT EXISTS idx_arkham_decks_user_last_used
  ON arkham_decks (user_id, last_used_at DESC);

COMMIT;
