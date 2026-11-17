-- Deploy arkham-horror-backend:add_admin_to_users to pg
-- requires: users

BEGIN;

-- `admin` is in Entity.User but was never in the plan: it only reached a
-- database through the hand-dumped setup.sql, so a database built from the
-- plan alone had no admin column and every admin handler failed on it.
ALTER TABLE users ADD COLUMN IF NOT EXISTS admin boolean DEFAULT false;

COMMIT;
