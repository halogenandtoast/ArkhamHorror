-- Deploy arkham-horror-backend:notifications to pg

BEGIN;

-- The notifications table predates the migration plan: it only ever existed in
-- the hand-dumped setup.sql, so a database built from the plan alone had no
-- table behind Base.Api.Handler.Notifications. This reproduces the shape the
-- dump carries (nullable body/created_at, integer identity) rather than the
-- stricter shape Entity.Notification implies, so an existing install is left
-- exactly as it is.
CREATE TABLE IF NOT EXISTS notifications (
  id serial PRIMARY KEY,
  body text,
  created_at timestamp without time zone
);

COMMIT;
