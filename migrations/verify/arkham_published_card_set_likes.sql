-- Verify arkham-horror-backend:arkham_published_card_set_likes on pg

BEGIN;

SELECT id, published_card_set_id, user_id, created_at
  FROM arkham_published_card_set_likes WHERE FALSE;

ROLLBACK;
