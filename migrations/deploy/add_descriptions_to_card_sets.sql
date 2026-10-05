-- Deploy arkham-horror-backend:add_descriptions_to_card_sets to pg
-- requires: arkham_custom_card_sets
-- requires: arkham_published_card_sets

BEGIN;

ALTER TABLE arkham_custom_card_sets ADD COLUMN IF NOT EXISTS description TEXT;
ALTER TABLE arkham_published_card_sets ADD COLUMN IF NOT EXISTS description TEXT;

COMMIT;
