-- Revert arkham-horror-backend:change_deck_url_to_nullable from pg

BEGIN;

ALTER TABLE public.arkham_decks
ALTER COLUMN url SET NOT NULL;

COMMIT;
