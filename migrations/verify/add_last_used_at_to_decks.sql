-- Verify arkham-horror-backend:add_last_used_at_to_decks on pg

BEGIN;

SELECT last_used_at FROM arkham_decks WHERE FALSE;

ROLLBACK;
