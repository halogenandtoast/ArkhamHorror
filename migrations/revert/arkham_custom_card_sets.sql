-- Revert arkham-horror-backend:arkham_custom_card_sets from pg

BEGIN;

DROP INDEX IF EXISTS idx_arkham_custom_cards_set;

ALTER TABLE arkham_custom_cards DROP COLUMN IF EXISTS custom_card_set_id;

DROP TABLE IF EXISTS arkham_custom_card_sets;

COMMIT;
