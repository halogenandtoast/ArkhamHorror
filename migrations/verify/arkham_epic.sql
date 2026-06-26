-- Verify arkham-horror-backend:arkham_epic on pg

BEGIN;

SELECT id, name, organizer_user_id, shared_state, total_investigators, step
  FROM arkham_epic_events
 WHERE FALSE;

SELECT id, arkham_epic_event_id, ordinal, arkham_game_id, seat_count
  FROM arkham_epic_groups
 WHERE FALSE;

SELECT id, arkham_epic_event_id, user_id, role, group_ordinal
  FROM arkham_epic_members
 WHERE FALSE;

SELECT id, arkham_epic_event_id, step, delta
  FROM arkham_epic_steps
 WHERE FALSE;

ROLLBACK;
