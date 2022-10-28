-- Deploy arkham-horror-backend:add_url_to_decks to pg
-- requires: arkham_decks

BEGIN;

ALTER TABLE arkham_decks
ADD COLUMN IF NOT EXISTS url text not null;

COMMIT;
