-- Verify arkham-horror-backend:arkham_card_set_shares on pg

BEGIN;

SELECT id, custom_card_set_id, token, created_at, updated_at
  FROM arkham_card_set_shares
 WHERE FALSE;

SELECT id, card_set_share_id, user_id, custom_card_set_id, subscribed, taken_at,
       revoked_at, created_at, updated_at
  FROM arkham_card_set_share_accesses
 WHERE FALSE;

SELECT subscribed FROM arkham_card_set_subscriptions WHERE FALSE;

ROLLBACK;
