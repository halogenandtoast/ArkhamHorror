-- Deploy arkham-horror-backend:allow_spectators to pg
-- requires: arkham_games
-- requires: users

BEGIN;

-- Hosting metadata, not a game rule: it stays out of `current_data` so that an
-- undo -- which replays an older copy of that blob -- cannot silently put the
-- game back in or out of the watch list.
ALTER TABLE arkham_games
  ADD COLUMN IF NOT EXISTS allow_spectators BOOLEAN NOT NULL DEFAULT FALSE;

-- The home page's watch list reads this and nothing else, so the index is
-- partial on the flag and ordered the way the list is. Opting in is rare, which
-- keeps the index small enough to be worth having on a table this wide.
CREATE INDEX IF NOT EXISTS arkham_games_allow_spectators_updated_at_idx
  ON arkham_games (updated_at DESC)
  WHERE allow_spectators;

-- What a new game starts as, for an account that would rather host in the open.
ALTER TABLE users
  ADD COLUMN IF NOT EXISTS allow_spectators_by_default BOOLEAN NOT NULL DEFAULT FALSE;

COMMIT;
