-- Verify arkham-horror-backend:allow_spectators on pg

BEGIN;

SELECT allow_spectators FROM arkham_games WHERE false;
SELECT allow_spectators_by_default FROM users WHERE false;

ROLLBACK;
