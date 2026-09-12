-- Deploy arkham-horror-backend:arkham_published_card_sets to pg
-- requires: users
-- requires: arkham_custom_card_sets

BEGIN;

-- A set its author has put in the marketplace. One row per published set, not
-- per version: this is the listing, and the versions hang off it.
--
-- custom_card_set_id is the author's own working copy, kept so publishing again
-- knows which listing to add a version to. It goes null rather than taking the
-- listing with it if the author deletes their copy -- people who subscribed to
-- it still have something to read.
CREATE TABLE IF NOT EXISTS arkham_published_card_sets (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  user_id bigint REFERENCES users (id) ON DELETE CASCADE NOT NULL,
  custom_card_set_id uuid REFERENCES arkham_custom_card_sets (id) ON DELETE SET NULL,
  name varchar NOT NULL,
  latest_version int NOT NULL,
  created_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL
);

-- One row per published version, holding that version's cards outright.
--
-- The cards are snapshotted rather than read back off the author's set, because
-- a version has to stay importable exactly as published: a subscriber who edits
-- their copy and wants the published one back has to be able to take it again,
-- and the author has meanwhile moved on.
CREATE TABLE IF NOT EXISTS arkham_published_card_set_versions (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  published_card_set_id uuid REFERENCES arkham_published_card_sets (id) ON DELETE CASCADE NOT NULL,
  version int NOT NULL,
  note varchar,
  name varchar NOT NULL,
  cards jsonb NOT NULL,
  created_at timestamptz NOT NULL,
  CONSTRAINT unique_published_card_set_version UNIQUE (published_card_set_id, version)
);

-- One of your sets following a published one, and which version it is on.
--
-- Its own table rather than two columns on arkham_custom_card_sets, because that
-- table is referenced by arkham_published_card_sets: pointing back the other way
-- would make the two entity modules import each other.
--
-- A row here is deleted, not updated, when the set is edited: what is in the set
-- is then no longer what was published, so it is no longer subscribed.
CREATE TABLE IF NOT EXISTS arkham_card_set_subscriptions (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  custom_card_set_id uuid REFERENCES arkham_custom_card_sets (id) ON DELETE CASCADE NOT NULL,
  published_card_set_id uuid REFERENCES arkham_published_card_sets (id) ON DELETE CASCADE NOT NULL,
  version int NOT NULL,
  created_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL,
  CONSTRAINT unique_card_set_subscription UNIQUE (custom_card_set_id)
);

-- The listing is read newest-first, and a subscriber's set looks up its source.
CREATE INDEX IF NOT EXISTS idx_arkham_published_card_sets_updated
  ON arkham_published_card_sets (updated_at DESC);

CREATE INDEX IF NOT EXISTS idx_arkham_card_set_subscriptions_published
  ON arkham_card_set_subscriptions (published_card_set_id);

COMMIT;
