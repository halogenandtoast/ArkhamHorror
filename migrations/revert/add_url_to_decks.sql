-- Revert arkham-horror-backend:add_url_to_decks from pg

BEGIN;

ALTER TABLE arkham_decks
DROP COLUMN url;

COMMIT;
