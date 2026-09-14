-- Deploy arkham-horror-backend:arkham_custom_card_sets to pg
-- requires: users
-- requires: arkham_custom_cards

BEGIN;

-- Named collections that own custom cards: the unit you build, export, and hand
-- to someone else. Grouping used to be a string each card carried in its own
-- def, which meant a set existed only as far as every one of its cards agreed
-- on the spelling, and throwing one away meant deleting its cards one at a
-- time.
--
-- source_code is the id of the pack an imported set came from (arkham.build
-- gives one), so importing that pack again replaces this set rather than making
-- a second copy of it. A set built here has none.

CREATE TABLE IF NOT EXISTS arkham_custom_card_sets (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  user_id bigint REFERENCES users (id) ON DELETE CASCADE NOT NULL,
  name varchar NOT NULL,
  source_code varchar,
  created_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL,
  CONSTRAINT unique_user_custom_card_set_name UNIQUE (user_id, name)
);

ALTER TABLE arkham_custom_cards
  ADD COLUMN IF NOT EXISTS custom_card_set_id uuid
  REFERENCES arkham_custom_card_sets (id) ON DELETE CASCADE;

-- Cards that predate sets are put into one named after whatever they were
-- claiming for themselves, so nobody's library arrives here ungrouped.
INSERT INTO arkham_custom_card_sets (user_id, name, created_at, updated_at)
SELECT DISTINCT
    c.user_id,
    COALESCE(NULLIF(TRIM(c.def -> 'meta' ->> 'set'), ''), 'Imported cards'),
    now(),
    now()
  FROM arkham_custom_cards c
ON CONFLICT ON CONSTRAINT unique_user_custom_card_set_name DO NOTHING;

UPDATE arkham_custom_cards c
   SET custom_card_set_id = s.id
  FROM arkham_custom_card_sets s
 WHERE s.user_id = c.user_id
   AND s.name = COALESCE(NULLIF(TRIM(c.def -> 'meta' ->> 'set'), ''), 'Imported cards')
   AND c.custom_card_set_id IS NULL;

-- The name a card carries is only a copy of the set's, but it is the one
-- anything looking at a def alone reads, so every card is given the name of the
-- set it was just filed under -- including the ones whose own spelling of it
-- (untrimmed, or absent entirely) is what put them there.
UPDATE arkham_custom_cards c
   SET def = jsonb_set(
         jsonb_set(c.def, '{meta}', COALESCE(c.def -> 'meta', '{}'::jsonb), true),
         '{meta,set}',
         to_jsonb(s.name),
         true
       )
  FROM arkham_custom_card_sets s
 WHERE s.id = c.custom_card_set_id;

-- Every card belongs to a set, now that every card has one.
ALTER TABLE arkham_custom_cards
  ALTER COLUMN custom_card_set_id SET NOT NULL;

-- Listing, counting and emptying a set all go by this.
CREATE INDEX IF NOT EXISTS idx_arkham_custom_cards_set
  ON arkham_custom_cards (custom_card_set_id);

COMMIT;
