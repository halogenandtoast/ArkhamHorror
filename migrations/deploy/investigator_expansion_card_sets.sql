-- Deploy arkham-horror-backend:investigator_expansion_card_sets to pg
-- requires: add_urls_to_card_sets

BEGIN;

-- An investigator expansion is a set that brings its own furniture: an icon the
-- marketplace files it under, tokens its cards place on themselves, and
-- keywords its cards print. The flag is what unlocks the three, so a plain set
-- is never asked about any of them.
ALTER TABLE arkham_custom_card_sets
  ADD COLUMN IF NOT EXISTS investigator_expansion BOOLEAN NOT NULL DEFAULT FALSE,
  ADD COLUMN IF NOT EXISTS icon TEXT,
  ADD COLUMN IF NOT EXISTS tokens JSONB NOT NULL DEFAULT '[]'::jsonb,
  ADD COLUMN IF NOT EXISTS keywords JSONB NOT NULL DEFAULT '[]'::jsonb;

-- The listing carries what a browser needs without loading the set: the icon it
-- is drawn with, and whether it is an expansion at all.
ALTER TABLE arkham_published_card_sets
  ADD COLUMN IF NOT EXISTS investigator_expansion BOOLEAN NOT NULL DEFAULT FALSE,
  ADD COLUMN IF NOT EXISTS icon TEXT;

COMMIT;
