-- Verify arkham-horror-backend:arkham_published_card_sets on pg

BEGIN;

SELECT id, user_id, custom_card_set_id, name, latest_version, created_at, updated_at
  FROM arkham_published_card_sets WHERE FALSE;

SELECT id, published_card_set_id, version, note, name, cards, created_at
  FROM arkham_published_card_set_versions WHERE FALSE;

SELECT id, custom_card_set_id, published_card_set_id, version, created_at, updated_at
  FROM arkham_card_set_subscriptions WHERE FALSE;

ROLLBACK;
