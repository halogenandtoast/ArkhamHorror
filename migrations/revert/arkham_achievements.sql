-- Revert arkham-horror-backend:arkham_achievements from pg

BEGIN;

DROP TABLE IF EXISTS arkham_achievements;

COMMIT;
