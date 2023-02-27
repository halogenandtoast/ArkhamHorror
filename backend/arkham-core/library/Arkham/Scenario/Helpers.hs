module Arkham.Scenario.Helpers
  ( module Arkham.Scenario.Helpers
  , module X
  ) where

import Arkham.Prelude

import Arkham.EncounterSet
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Matcher
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Game.Helpers as X
import Arkham.Helpers

getHasRecordOrStandalone
  :: HasGame m => CampaignLogKey
  -> Bool
  -> m Bool
getHasRecordOrStandalone key def = do
  standalone <- selectNone TheCampaign
  if standalone then pure def else getHasRecord key

buildEncounterDeck :: CardGen m => [EncounterSet] -> m (Deck EncounterCard)
buildEncounterDeck = buildEncounterDeckWith id

buildEncounterDeckExcluding
  :: CardGen m => [SomeCardDef] -> [EncounterSet] -> m (Deck EncounterCard)
buildEncounterDeckExcluding defs =
  buildEncounterDeckWith (filter ((`notElem` defs) . toCardDef))

excludeDoubleSided :: [EncounterCard] -> [EncounterCard]
excludeDoubleSided = filter (not . withCardDef cdDoubleSided)

excludeBSides :: [EncounterCard] -> [EncounterCard]
excludeBSides = filter (not . isSuffixOf "b" . unCardCode . toCardCode)

buildEncounterDeckWith
  :: CardGen m => ([EncounterCard] -> [EncounterCard])
  -> [EncounterSet]
  -> m (Deck EncounterCard)
buildEncounterDeckWith f encounterSets =
  Deck
    <$> (shuffleM
        . f
        . excludeBSides
        . excludeDoubleSided
        . concat
        =<< traverse gatherEncounterSet encounterSets
        )
