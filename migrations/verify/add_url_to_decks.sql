-- Verify arkham-horror-backend:add_url_to_decks on pg

BEGIN;

SELECT url FROM arkham_decks WHERE false;

ROLLBACK;
