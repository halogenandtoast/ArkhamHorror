-- Deploy arkham-horror-backend:arkham_custom_cards to pg
-- requires: users

BEGIN;

-- Cards built in the card builder, kept against their author's account so they
-- outlive any one game. card_code is the code minted when the card was first
-- created and is what games refer to it by, so saving an edit replaces the row
-- rather than adding one. art is a URL or an inlined data URI, kept apart from
-- def so listing a library does not have to carry it.

CREATE TABLE IF NOT EXISTS arkham_custom_cards (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  user_id bigint REFERENCES users (id) ON DELETE CASCADE NOT NULL,
  card_code varchar NOT NULL,
  def jsonb NOT NULL,
  art text,
  created_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL,
  CONSTRAINT unique_user_custom_card UNIQUE (user_id, card_code)
);

COMMIT;
