-- Revert arkham-horror-backend:arkham_ml_decisions from pg

BEGIN;

DROP TABLE IF EXISTS arkham_ml_decisions;

COMMIT;
