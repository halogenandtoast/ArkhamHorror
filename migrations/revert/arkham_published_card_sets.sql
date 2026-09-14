-- Revert arkham-horror-backend:arkham_published_card_sets from pg

BEGIN;

DROP TABLE IF EXISTS arkham_card_set_subscriptions;
DROP TABLE IF EXISTS arkham_published_card_set_versions;
DROP TABLE IF EXISTS arkham_published_card_sets;

COMMIT;
