-- Deploy arkham-horror-backend:arkham_deck_overlay to pg
-- requires: arkham_decks

BEGIN;

-- A deck's overlay: the custom investigator and card changes laid over the
-- decklist. Kept beside the list rather than folded into it, so the original
-- deck stays intact and the overlay can be lifted again.

ALTER TABLE arkham_decks ADD COLUMN IF NOT EXISTS overlay jsonb;

COMMIT;
