-- Deploy arkham-horror-backend:arkham_epic to pg
-- requires: arkham_games
-- requires: users

BEGIN;

-- Epic Multiplayer: an event owns N otherwise-independent group games and a
-- small amount of shared state. The central arkham_games table is left
-- untouched; group games are referenced by FK from arkham_epic_groups.

CREATE TABLE IF NOT EXISTS arkham_epic_events (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  name text NOT NULL,
  organizer_user_id bigint REFERENCES users (id) ON DELETE CASCADE NOT NULL,
  scenario_id text,
  campaign_id text,
  difficulty text NOT NULL,
  shared_state jsonb NOT NULL,
  total_investigators integer NOT NULL,
  step integer NOT NULL,
  created_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL
);

CREATE TABLE IF NOT EXISTS arkham_epic_groups (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  arkham_epic_event_id uuid REFERENCES arkham_epic_events (id) ON DELETE CASCADE NOT NULL,
  ordinal integer NOT NULL,
  arkham_game_id uuid REFERENCES arkham_games (id) ON DELETE CASCADE,
  name text NOT NULL,
  seat_count integer NOT NULL,
  UNIQUE (arkham_epic_event_id, ordinal)
);

CREATE TABLE IF NOT EXISTS arkham_epic_members (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  arkham_epic_event_id uuid REFERENCES arkham_epic_events (id) ON DELETE CASCADE NOT NULL,
  user_id bigint REFERENCES users (id) ON DELETE CASCADE NOT NULL,
  role text NOT NULL,
  group_ordinal integer,
  UNIQUE (arkham_epic_event_id, user_id, role)
);

CREATE TABLE IF NOT EXISTS arkham_epic_steps (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  arkham_epic_event_id uuid REFERENCES arkham_epic_events (id) ON DELETE CASCADE NOT NULL,
  step integer NOT NULL,
  arkham_game_id uuid,
  game_step integer,
  delta jsonb NOT NULL,
  created_at timestamptz NOT NULL
);

CREATE INDEX IF NOT EXISTS arkham_epic_groups_event_idx ON arkham_epic_groups (arkham_epic_event_id);
CREATE INDEX IF NOT EXISTS arkham_epic_groups_game_idx ON arkham_epic_groups (arkham_game_id);
CREATE INDEX IF NOT EXISTS arkham_epic_members_event_idx ON arkham_epic_members (arkham_epic_event_id);
CREATE UNIQUE INDEX IF NOT EXISTS arkham_epic_steps_event_step_idx ON arkham_epic_steps (arkham_epic_event_id, step);

COMMIT;
