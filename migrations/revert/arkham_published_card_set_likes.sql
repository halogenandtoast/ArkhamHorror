-- Revert arkham-horror-backend:arkham_published_card_set_likes from pg

BEGIN;

DROP TABLE IF EXISTS arkham_published_card_set_likes;

COMMIT;
