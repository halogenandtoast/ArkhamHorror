-- Verify arkham-horror-backend:add_urls_to_card_sets on pg

BEGIN;

SELECT url FROM arkham_custom_card_sets WHERE false;
SELECT url FROM arkham_published_card_sets WHERE false;

ROLLBACK;
