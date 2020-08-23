-- Revert arkham-horror-backend:arkham_pending_games from pg

BEGIN;

DROP TABLE arkham_pending_games;

COMMIT;
