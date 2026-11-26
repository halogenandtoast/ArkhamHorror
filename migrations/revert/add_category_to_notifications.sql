-- Revert arkham-horror-backend:add_category_to_notifications from pg

BEGIN;

ALTER TABLE notifications
  DROP COLUMN IF EXISTS category;

COMMIT;
