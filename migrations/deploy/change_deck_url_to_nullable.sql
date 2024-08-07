-- Deploy arkham-horror-backend:change_deck_url_to_nullable to pg

BEGIN;

ALTER TABLE arkham_decks
ALTER COLUMN url DROP NOT NULL;

COMMIT;
