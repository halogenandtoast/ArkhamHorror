-- Deploy arkham-horror-backend:add_urls_to_card_sets to pg
-- requires: add_descriptions_to_card_sets

BEGIN;

ALTER TABLE arkham_custom_card_sets ADD COLUMN IF NOT EXISTS url TEXT;
ALTER TABLE arkham_published_card_sets ADD COLUMN IF NOT EXISTS url TEXT;

COMMIT;
