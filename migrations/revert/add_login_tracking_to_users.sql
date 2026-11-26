-- Revert arkham-horror-backend:add_login_tracking_to_users from pg

BEGIN;

ALTER TABLE users
  DROP COLUMN IF EXISTS last_login_at,
  DROP COLUMN IF EXISTS last_seen_at;

COMMIT;
