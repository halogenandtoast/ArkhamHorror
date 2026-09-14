-- Verify arkham-horror-backend:arkham_custom_card_sets on pg

BEGIN;

SELECT id, user_id, name, source_code, created_at, updated_at
  FROM arkham_custom_card_sets
 WHERE FALSE;

SELECT custom_card_set_id
  FROM arkham_custom_cards
 WHERE FALSE;

ROLLBACK;
