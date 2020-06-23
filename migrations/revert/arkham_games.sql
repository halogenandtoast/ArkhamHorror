-- Revert arkham-horror-backend:arkham_games from pg

BEGIN;

DROP TABLE arkham_games;

COMMIT;
