-- Deploy arkham-horror-backend:arkham_published_card_set_likes to pg
-- requires: users
-- requires: arkham_published_card_sets

BEGIN;

-- One person saying they liked a published set. A row is the like; deleting it is
-- taking it back, so there is no state to keep in step and no way to like twice.
CREATE TABLE IF NOT EXISTS arkham_published_card_set_likes (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  published_card_set_id uuid REFERENCES arkham_published_card_sets (id) ON DELETE CASCADE NOT NULL,
  user_id bigint REFERENCES users (id) ON DELETE CASCADE NOT NULL,
  created_at timestamptz NOT NULL,
  CONSTRAINT unique_published_card_set_like UNIQUE (published_card_set_id, user_id)
);

-- Counting a set's likes, and ordering the marketplace by them.
CREATE INDEX IF NOT EXISTS idx_arkham_published_card_set_likes_set
  ON arkham_published_card_set_likes (published_card_set_id);

COMMIT;
