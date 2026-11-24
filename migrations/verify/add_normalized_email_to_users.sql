-- Verify arkham-horror-backend:add_normalized_email_to_users on pg

BEGIN;

SELECT 1 / CASE WHEN
  EXISTS (
    SELECT 1
      FROM information_schema.columns
     WHERE table_name = 'users'
       AND column_name = 'email_normalized'
       AND is_nullable = 'NO'
  )
  AND EXISTS (
    SELECT 1
      FROM pg_indexes
     WHERE schemaname = 'public'
       AND indexname = 'users_email_normalized_key'
       AND indexdef LIKE 'CREATE UNIQUE INDEX%'
  )
  AND NOT EXISTS (
    SELECT 1
      FROM users
     WHERE email_normalized <> lower(btrim(email))
  )
THEN 1 ELSE 0 END;

ROLLBACK;
