-- Revert arkham-horror-backend:arkham_custom_cards from pg

BEGIN;

DROP TABLE IF EXISTS arkham_custom_cards;

COMMIT;
