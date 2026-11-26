-- Deploy arkham-horror-backend:add_category_to_notifications to pg
-- requires: notifications

BEGIN;

-- What kind of announcement a notification is, which is what the home page
-- stamps it with: 'important', 'feature' or 'content'.
--
-- Nullable, and that is a value rather than an omission: an announcement with
-- no category is an ordinary one and is drawn with no stamp. Every row written
-- before this column existed is one of those, which is why there is no default
-- and no backfill -- guessing a category for an announcement already up would
-- restyle it under somebody who is reading it.
--
-- No CHECK constraint: the handler is the one place a value can enter, and it
-- answers an unknown category with a 400. A constraint here would turn a
-- future fourth category into a migration before the code that writes it can
-- deploy, and the home page already ignores a category it does not know.
ALTER TABLE notifications
  ADD COLUMN IF NOT EXISTS category TEXT;

COMMIT;
