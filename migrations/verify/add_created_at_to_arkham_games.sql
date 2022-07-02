-- Verify arkham-horror-backend:add_created_at_to_arkham_games on pg

BEGIN;

SELECT created_at, updated_at From arkham_games WHERE false;

ROLLBACK;
