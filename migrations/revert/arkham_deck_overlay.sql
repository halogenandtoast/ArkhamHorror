-- Revert arkham-horror-backend:arkham_deck_overlay from pg

BEGIN;

ALTER TABLE arkham_decks DROP COLUMN IF EXISTS overlay;

COMMIT;
