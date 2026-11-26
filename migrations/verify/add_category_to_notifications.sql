-- Verify arkham-horror-backend:add_category_to_notifications on pg

BEGIN;

SELECT category FROM notifications WHERE false;

ROLLBACK;
