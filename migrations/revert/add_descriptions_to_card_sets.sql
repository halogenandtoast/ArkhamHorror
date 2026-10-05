-- Revert arkham-horror-backend:add_descriptions_to_card_sets from pg

BEGIN;

ALTER TABLE arkham_custom_card_sets DROP COLUMN IF EXISTS description;
ALTER TABLE arkham_published_card_sets DROP COLUMN IF EXISTS description;

COMMIT;
