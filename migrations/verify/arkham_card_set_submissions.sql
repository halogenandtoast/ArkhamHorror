-- Verify arkham-horror-backend:arkham_card_set_submissions on pg

BEGIN;

SELECT id, published_card_set_id, version_id, user_id, version, status, notify,
       reason, reviewed_by_user_id, reviewed_at, created_at, updated_at
  FROM arkham_card_set_submissions
 WHERE FALSE;

SELECT approved_version FROM arkham_published_card_sets WHERE FALSE;

ROLLBACK;
