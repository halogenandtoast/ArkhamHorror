-- Revert arkham-horror-backend:investigator_expansion_card_sets from pg

BEGIN;

ALTER TABLE arkham_custom_card_sets
  DROP COLUMN IF EXISTS investigator_expansion,
  DROP COLUMN IF EXISTS icon,
  DROP COLUMN IF EXISTS tokens,
  DROP COLUMN IF EXISTS keywords;

ALTER TABLE arkham_published_card_sets
  DROP COLUMN IF EXISTS investigator_expansion,
  DROP COLUMN IF EXISTS icon;

COMMIT;
