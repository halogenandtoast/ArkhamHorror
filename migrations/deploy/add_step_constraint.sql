BEGIN;

CREATE OR REPLACE FUNCTION check_step_before_delete() RETURNS TRIGGER AS $$
BEGIN
    -- Raise an exception if trying to delete a step that is equal to or less than the current step in the game
    IF OLD.step <= (
        SELECT step
        FROM arkham_games
        WHERE id = OLD.arkham_game_id
    ) THEN
        RAISE EXCEPTION 'Cannot delete step % because it is equal to or less than the current step in the corresponding game.', OLD.step;
    END IF;
    
    RETURN OLD;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER prevent_invalid_step_deletion
BEFORE DELETE ON arkham_steps
FOR EACH ROW
EXECUTE FUNCTION check_step_before_delete();

COMMIT;
