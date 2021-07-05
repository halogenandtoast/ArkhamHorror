module Arkham.Types.Scenario.Helpers
  ( module Arkham.Types.Scenario.Helpers
  , module X
  ) where

import Arkham.Prelude

import Arkham.EncounterSet
import Arkham.Types.CampaignId
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers as X

getHasRecordOrStandalone
  :: (MonadReader env m, HasRecord env, HasId (Maybe CampaignId) env ())
  => CampaignLogKey
  -> Bool
  -> m Bool
getHasRecordOrStandalone key def = do
  standalone <- isNothing <$> getId @(Maybe CampaignId) ()
  if standalone then pure def else getHasRecord key

buildEncounterDeck :: MonadRandom m => [EncounterSet] -> m [EncounterCard]
buildEncounterDeck = buildEncounterDeckWith id

buildEncounterDeckExcluding
  :: MonadRandom m => [CardDef] -> [EncounterSet] -> m [EncounterCard]
buildEncounterDeckExcluding defs =
  buildEncounterDeckWith (filter ((`notElem` defs) . toCardDef))

excludeDoubleSided :: [EncounterCard] -> [EncounterCard]
excludeDoubleSided = filter (not . cdDoubleSided . toCardDef)

buildEncounterDeckWith
  :: MonadRandom m
  => ([EncounterCard] -> [EncounterCard])
  -> [EncounterSet]
  -> m [EncounterCard]
buildEncounterDeckWith f encounterSets =
  shuffleM
    . f
    . excludeDoubleSided
    . concat
    =<< traverse gatherEncounterSet encounterSets
