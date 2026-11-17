-- Verify arkham-horror-backend:add_admin_to_users on pg

BEGIN;

SELECT admin FROM users WHERE false;

ROLLBACK;
