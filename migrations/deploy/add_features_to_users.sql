-- Deploy arkham-horror-backend:add_features_to_users to pg
-- requires: users

BEGIN;

ALTER TABLE users ADD COLUMN IF NOT EXISTS features TEXT NOT NULL DEFAULT '';

COMMIT;
