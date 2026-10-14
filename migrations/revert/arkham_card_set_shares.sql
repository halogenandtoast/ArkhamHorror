-- Revert arkham-horror-backend:arkham_card_set_shares from pg

BEGIN;

DROP INDEX IF EXISTS idx_arkham_card_set_share_accesses_user;
DROP INDEX IF EXISTS idx_arkham_card_set_share_accesses_share;

DROP TABLE IF EXISTS arkham_card_set_share_accesses;
DROP TABLE IF EXISTS arkham_card_set_shares;

COMMIT;
