-- Revert arkham-horror-backend:add_normalized_email_to_users from pg

BEGIN;

DROP INDEX users_email_normalized_key;

ALTER TABLE users
  DROP COLUMN email_normalized;

COMMIT;
