-- Deploy arkham-horror-backend:arkham_card_set_shares to pg
-- requires: arkham_custom_card_sets

BEGIN;

-- Handing an unpublished set to a few people.
--
-- The marketplace is the other way to do this, and it is the wrong one for a set
-- that is not finished: it is public, every version is reviewed before anyone
-- can take it, and what you want while playtesting is for the people trying the
-- cards to get whatever you changed this afternoon.
--
-- A share is a capability url. There is one per set -- a link you send to
-- whoever you want rather than an invitation per person -- so turning sharing
-- off or minting a new token is how everyone is cut off at once, and the row
-- below is how one person is.
--
-- `token` is stored as given rather than hashed, unlike an api key: the owner
-- has to be able to see the link again to send it to a second friend, so there
-- is nothing to be gained by being unable to read it.
CREATE TABLE IF NOT EXISTS arkham_card_set_shares (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  custom_card_set_id uuid NOT NULL
    REFERENCES arkham_custom_card_sets (id) ON DELETE CASCADE,
  token varchar NOT NULL,
  created_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL,
  CONSTRAINT unique_card_set_share UNIQUE (custom_card_set_id),
  CONSTRAINT unique_card_set_share_token UNIQUE (token)
);

-- One person the link reached, which is the whole of "who has access": a share
-- is a url, so the only record of who holds it is who has opened it.
--
-- `custom_card_set_id` is their own copy of the set, so taking the author's
-- newest cards replaces the copy they have rather than leaving them with two.
-- It goes null rather than taking the row with it if they delete that copy,
-- because they still have access and can take it again.
--
-- `taken_at` is when that copy was last written. Everything the recipient's page
-- says is read off it against the two sets' `updated_at`: the author has changes
-- if theirs is newer, and the copy has been edited since if the copy is. Neither
-- is a flag anybody maintains, which is why the author editing a card has to
-- touch the set row (see `postApiV1ArkhamCustomCardsR`).
--
-- `revoked_at` is the author cutting this one person off without disturbing the
-- link everyone else is holding. It does not delete anything of theirs: the
-- cards they have taken are in their library and are theirs, and all this stops
-- is reading the set and taking it again.
CREATE TABLE IF NOT EXISTS arkham_card_set_share_accesses (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  card_set_share_id uuid NOT NULL
    REFERENCES arkham_card_set_shares (id) ON DELETE CASCADE,
  user_id bigint NOT NULL REFERENCES users (id) ON DELETE CASCADE,
  custom_card_set_id uuid
    REFERENCES arkham_custom_card_sets (id) ON DELETE SET NULL,
  taken_at timestamptz,
  revoked_at timestamptz,
  created_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL,
  CONSTRAINT unique_card_set_share_access UNIQUE (card_set_share_id, user_id)
);

-- "Who has access", oldest first, which is the one query the table exists for.
CREATE INDEX IF NOT EXISTS idx_arkham_card_set_share_accesses_share
  ON arkham_card_set_share_accesses (card_set_share_id, created_at);

-- A recipient asking where they stand with a set they have been given, which is
-- every load of their own builder.
CREATE INDEX IF NOT EXISTS idx_arkham_card_set_share_accesses_user
  ON arkham_card_set_share_accesses (user_id);

COMMIT;
