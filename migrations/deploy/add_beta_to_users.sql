-- Deploy arkham-horror-backend:add_beta_to_users to pg
-- requires: users

BEGIN;

ALTER TABLE users ADD COLUMN IF NOT EXISTS beta BOOLEAN NOT NULL DEFAULT FALSE;
-- XXX Add DDLs here.

COMMIT;
