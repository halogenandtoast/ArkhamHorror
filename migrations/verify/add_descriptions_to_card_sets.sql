-- Verify arkham-horror-backend:add_descriptions_to_card_sets on pg

BEGIN;

SELECT description FROM arkham_custom_card_sets WHERE false;
SELECT description FROM arkham_published_card_sets WHERE false;

ROLLBACK;
