-- Verify arkham-horror-backend:arkham_deck_overlay on pg

BEGIN;

SELECT overlay FROM arkham_decks WHERE FALSE;

ROLLBACK;
