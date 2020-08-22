-- Revert arkham-horror-backend:arkham_players from pg

BEGIN;

DROP TABLE arkham_players;

COMMIT;
