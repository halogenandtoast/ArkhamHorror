-- Deploy arkham-horror-backend:arkham_game_archives to pg
-- requires: arkham_games
-- requires: users

BEGIN;

-- Where a game's undo history went when it was offloaded to object storage.
--
-- `arkham_steps` is the database: 94 of 98 GB, and nothing ever pruned it. Most
-- of that belongs to games nobody has touched in months, whose steps exist only
-- so that an undo nobody is going to ask for would work. This table is the
-- record of moving that history out to S3 and back.
--
-- One row per game, not one per archive: a game is either offloaded or it is
-- not, and re-archiving a restored game reuses its row. `state` is the whole
-- lifecycle -- 'archived', 'restoring', 'restored', 'failed' -- so the undo
-- endpoints can answer "your history is in cold storage" from a single indexed
-- lookup rather than by counting step rows.
--
-- `steps` and `bytes` are kept after a restore rather than cleared, because the
-- panel that justifies this whole mechanism is asking how much it reclaimed.
CREATE TABLE IF NOT EXISTS arkham_game_archives (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  arkham_game_id uuid NOT NULL REFERENCES arkham_games(id) ON DELETE CASCADE,
  state text NOT NULL,
  bucket text NOT NULL,
  object_key text NOT NULL,
  steps int NOT NULL,
  -- The game's step at archive time. Restoring is "put back everything up to
  -- here"; anything above it was played after the archive was taken.
  top_step int NOT NULL,
  -- Size of the uploaded object, which is the gzipped debug export.
  bytes bigint NOT NULL,
  archived_at timestamptz NOT NULL,
  -- How idle the game was when it was taken, kept so the retention window can
  -- be argued about later with the evidence in hand.
  game_updated_at timestamptz NOT NULL,
  requested_at timestamptz,
  requested_by bigint REFERENCES users(id) ON DELETE SET NULL,
  restored_at timestamptz,
  error_message text,
  CONSTRAINT unique_game_archive UNIQUE (arkham_game_id)
);

-- The sweep asks for candidates by state, and the restore worker asks for rows
-- stuck in 'restoring'. Both are a small slice of the table.
CREATE INDEX IF NOT EXISTS idx_arkham_game_archives_state
  ON arkham_game_archives (state);

CREATE INDEX IF NOT EXISTS idx_arkham_game_archives_archived_at
  ON arkham_game_archives (archived_at DESC);

-- Archiving has to delete every step a game has, including the one it is
-- sitting on, and the delete guards exist precisely to stop that. Rather than
-- `ALTER TABLE ... DISABLE TRIGGER` -- which is DDL, takes an ACCESS EXCLUSIVE
-- lock on the hottest table in the application, and lifts the guard for every
-- other session for the duration -- the guards learn one exemption: a
-- transaction that has announced itself as the archiver.
--
-- `set_config(..., true)` is transaction-local, so the exemption cannot outlive
-- the statement that needed it, and no other session can see it.
--
-- Both functions are replaced because this schema has carried both over time
-- and which one is actually attached to a trigger differs between deployments.
-- Replacing an unused function is harmless; missing the attached one is not.
CREATE OR REPLACE FUNCTION check_no_higher_step_exists() RETURNS TRIGGER AS $$
BEGIN
    IF coalesce(current_setting('arkham.archiving', true), '') = 'on' THEN
        RETURN OLD;
    END IF;

    -- Check if the game still exists
    IF EXISTS (
        SELECT 1
        FROM arkham_games
        WHERE id = OLD.arkham_game_id
    ) THEN
        -- If the game exists, check if there is any step with a higher step number for the same game
        IF EXISTS (
            SELECT 1
            FROM arkham_steps
            WHERE arkham_game_id = OLD.arkham_game_id
              AND step > OLD.step
        ) THEN
            RAISE EXCEPTION 'Cannot delete step % because a higher step exists for the same game.', OLD.step;
        END IF;
    END IF;

    RETURN OLD;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION check_step_before_delete() RETURNS TRIGGER AS $$
BEGIN
    IF coalesce(current_setting('arkham.archiving', true), '') = 'on' THEN
        RETURN OLD;
    END IF;

    -- Raise an exception if trying to delete a step that is equal to or less than the current step in the game
    IF OLD.step <= (
        SELECT step
        FROM arkham_games
        WHERE id = OLD.arkham_game_id
    ) THEN
        RAISE EXCEPTION 'Cannot delete step % because it is equal to or less than the current step in the corresponding game.', OLD.step;
    END IF;

    RETURN OLD;
END;
$$ LANGUAGE plpgsql;

-- The same exemption on the way back in. A restore inserts ascending, so the
-- ordinary rule would be satisfied anyway -- but only if the chain is whole. A
-- game that was played after being archived has steps above the archive and
-- none below, and inserting into that gap is exactly what a restore is.
CREATE OR REPLACE FUNCTION enforce_step_order_per_game() RETURNS TRIGGER AS $$
BEGIN
    IF coalesce(current_setting('arkham.archiving', true), '') = 'on' THEN
        RETURN NEW;
    END IF;

    -- Step 0 always starts a chain.
    IF NEW.step = 0 THEN
        RETURN NEW;
    END IF;

    -- Normal case: the immediately preceding step exists.
    IF EXISTS (
        SELECT 1 FROM arkham_steps
        WHERE arkham_game_id = NEW.arkham_game_id
          AND step = NEW.step - 1
    ) THEN
        RETURN NEW;
    END IF;

    -- Recovery case: the game has no recorded steps at all (its undo history
    -- was pruned or never persisted). Allow this insert to start a fresh
    -- contiguous chain from the game's current step instead of wedging the
    -- game so no action can ever be taken again.
    IF NOT EXISTS (
        SELECT 1 FROM arkham_steps
        WHERE arkham_game_id = NEW.arkham_game_id
    ) THEN
        RETURN NEW;
    END IF;

    RAISE EXCEPTION 'Cannot insert step % for game % without step %', NEW.step, NEW.arkham_game_id, NEW.step - 1;
END;
$$ LANGUAGE plpgsql;

COMMIT;
