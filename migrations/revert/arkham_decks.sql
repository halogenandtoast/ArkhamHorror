-- Revert arkham-horror-backend:akrham_decks from pg

BEGIN;

DROP TABLE arkham_decks;

COMMIT;
