BEGIN;

ALTER TABLE arkham_players ADD COLUMN uuid uuid DEFAULT uuid_generate_v4();
ALTER TABLE arkham_players DROP COLUMN id;
ALTER TABLE arkham_players RENAME COLUMN uuid to id;


COMMIT;
