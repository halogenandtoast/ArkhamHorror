-- Verify arkham-horror-backend:add_login_tracking_to_users on pg

BEGIN;

SELECT last_login_at, last_seen_at FROM users WHERE false;

ROLLBACK;
