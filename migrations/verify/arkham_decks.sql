-- Verify arkham-horror-backend:akrham_decks on pg

BEGIN;

SELECT id, user_id, name, investigator_name, list
  FROM arkham_decks
 WHERE FALSE;

ROLLBACK;
