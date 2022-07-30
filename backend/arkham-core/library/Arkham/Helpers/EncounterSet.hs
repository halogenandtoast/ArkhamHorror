module Arkham.Helpers.EncounterSet where

import Arkham.Prelude

import Arkham.Card.CardDef
import Arkham.Card.EncounterCard
import Arkham.EncounterCard
import Arkham.EncounterSet

gatherEncounterSet :: MonadRandom m => EncounterSet -> m [EncounterCard]
gatherEncounterSet encounterSet = concat <$> for
  defs
  \def -> traverse genEncounterCard
    $ replicate (fromMaybe 0 (cdEncounterSetQuantity def)) def
 where
  defs =
    filter ((== Just encounterSet) . cdEncounterSet) $ toList allEncounterCards

