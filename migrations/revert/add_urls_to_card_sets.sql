-- Revert arkham-horror-backend:add_urls_to_card_sets from pg

BEGIN;

ALTER TABLE arkham_custom_card_sets DROP COLUMN IF EXISTS url;
ALTER TABLE arkham_published_card_sets DROP COLUMN IF EXISTS url;

COMMIT;
