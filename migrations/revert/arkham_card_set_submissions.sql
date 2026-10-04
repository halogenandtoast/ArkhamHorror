-- Revert arkham-horror-backend:arkham_card_set_submissions from pg

BEGIN;

ALTER TABLE arkham_published_card_sets DROP COLUMN IF EXISTS approved_version;

DROP INDEX IF EXISTS idx_arkham_card_set_submissions_set;
DROP INDEX IF EXISTS idx_arkham_card_set_submissions_status;

DROP TABLE IF EXISTS arkham_card_set_submissions;

COMMIT;
