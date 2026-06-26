-- Revert arkham-horror-backend:arkham_epic from pg

BEGIN;

DROP TABLE IF EXISTS arkham_epic_steps;
DROP TABLE IF EXISTS arkham_epic_members;
DROP TABLE IF EXISTS arkham_epic_groups;
DROP TABLE IF EXISTS arkham_epic_events;

COMMIT;
