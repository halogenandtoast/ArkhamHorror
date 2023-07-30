-- Deploy arkham-horror-backend:add_cascades to pg

BEGIN;

ALTER TABLE arkham_players
  DROP CONSTRAINT IF EXISTS arkham_players_arkham_game_id_fkey,
  DROP CONSTRAINT IF EXISTS arkham_players_user_id_fkey;

ALTER TABLE arkham_players
  ADD CONSTRAINT arkham_players_arkham_game_id_fkey
    FOREIGN KEY (arkham_game_id)
    REFERENCES arkham_games (id)
    ON DELETE CASCADE,
  ADD CONSTRAINT arkham_players_user_id_fkey
    FOREIGN KEY (user_id)
    REFERENCES users (id)
    ON DELETE CASCADE;

ALTER TABLE arkham_steps
  DROP CONSTRAINT IF EXISTS arkham_steps_arkham_game_id_fkey;

ALTER TABLE arkham_steps
  ADD CONSTRAINT arkham_steps_arkham_game_id_fkey
    FOREIGN KEY (arkham_game_id)
    REFERENCES arkham_games (id)
    ON DELETE CASCADE;

ALTER TABLE arkham_log_entries
  DROP CONSTRAINT IF EXISTS arkham_log_entries_arkham_game_id_fkey;

ALTER TABLE arkham_log_entries
  ADD CONSTRAINT arkham_log_entries_arkham_game_id_fkey
    FOREIGN KEY (arkham_game_id)
    REFERENCES arkham_games (id)
    ON DELETE CASCADE;

COMMIT;
