-- Verify arkham-horror-backend:investigator_expansion_card_sets on pg

BEGIN;

SELECT investigator_expansion, icon, tokens, keywords FROM arkham_custom_card_sets WHERE false;
SELECT investigator_expansion, icon FROM arkham_published_card_sets WHERE false;

ROLLBACK;
