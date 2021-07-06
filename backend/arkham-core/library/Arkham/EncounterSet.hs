module Arkham.EncounterSet
  ( module X
  , gatherEncounterSet
  )
where

import Arkham.Prelude

import Arkham.EncounterCard
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.EncounterCard
import Arkham.Types.EncounterSet as X

gatherEncounterSet :: MonadRandom m => EncounterSet -> m [EncounterCard]
gatherEncounterSet encounterSet = concat <$> for
  defs
  \def -> traverse genEncounterCard
    $ replicate (fromMaybe 0 (cdEncounterSetQuantity def)) def
 where
  defs =
    filter ((== Just encounterSet) . cdEncounterSet) $ toList allEncounterCards
