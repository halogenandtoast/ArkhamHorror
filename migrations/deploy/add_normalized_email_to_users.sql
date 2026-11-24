-- Deploy arkham-horror-backend:add_normalized_email_to_users to pg
-- requires: users

BEGIN;

DO $$
BEGIN
  IF EXISTS (
    SELECT 1
      FROM users
     GROUP BY lower(btrim(email))
    HAVING count(*) > 1
  ) THEN
    RAISE EXCEPTION
      'Cannot add normalized email: resolve users whose emails differ only by case or surrounding whitespace first';
  END IF;
END
$$;

ALTER TABLE users
  ADD COLUMN email_normalized character varying;

UPDATE users
   SET email_normalized = lower(btrim(email));

ALTER TABLE users
  ALTER COLUMN email_normalized SET NOT NULL;

CREATE UNIQUE INDEX users_email_normalized_key
  ON users (email_normalized);

COMMIT;
