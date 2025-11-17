module Arkham.Helpers.EncounterSet where

import Arkham.Prelude

import Arkham.Card
import Arkham.EncounterCard
import Arkham.EncounterSet

gatherEncounterSet :: CardGen m => EncounterSet -> m [EncounterCard]
gatherEncounterSet encounterSet =
  concat <$> for
    defs
    \def ->
      traverse genEncounterCard
        $ replicate (fromMaybe 0 (cdEncounterSetQuantity def)) def
 where
  defs =
    filter (and . sequence [not . hasBSide, (== Just encounterSet) . cdEncounterSet]) $ toList allEncounterCards
  hasBSide = and . sequence [isDoubleSided, isSuffixOf "b" . unCardCode . toCardCode]

  -- Location cards are weird because they are always double sided, but when we
  -- gather cards we want to include them even when the suffix is a "b", in these
  -- cases we just look to ensure it's not the other side of a card.
  isDoubleSided =
    or
      . sequence [and . sequence [cdDoubleSided, (/= LocationType) . cdCardType], isJust . cdOtherSide]
      . toCardDef

