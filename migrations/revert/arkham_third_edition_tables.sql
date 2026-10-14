-- Revert arkham-horror-backend:arkham_third_edition_tables from pg

BEGIN;

DROP TABLE IF EXISTS arkham_third_edition_history;
DROP TABLE IF EXISTS arkham_third_edition_tables;

COMMIT;
