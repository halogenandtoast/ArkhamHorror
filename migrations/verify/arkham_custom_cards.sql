-- Verify arkham-horror-backend:arkham_custom_cards on pg

BEGIN;

SELECT id, user_id, card_code, def, art, created_at, updated_at
  FROM arkham_custom_cards
 WHERE FALSE;

ROLLBACK;
