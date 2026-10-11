-- Revert arkham-horror-backend:arkham_game_archives from pg

BEGIN;

DROP TABLE IF EXISTS arkham_game_archives;

-- The trigger functions keep their archiver exemption. Reverting it would mean
-- restating three function bodies that this migration did not author, and the
-- exemption is inert without a transaction that sets `arkham.archiving`.

COMMIT;
