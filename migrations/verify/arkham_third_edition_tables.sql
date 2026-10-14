-- Verify arkham-horror-backend:arkham_third_edition_tables on pg

BEGIN;

SELECT id, host_id, started, version, payload, created_at, updated_at
  FROM arkham_third_edition_tables
 WHERE FALSE;

SELECT id, table_id, game, created_at
  FROM arkham_third_edition_history
 WHERE FALSE;

ROLLBACK;
