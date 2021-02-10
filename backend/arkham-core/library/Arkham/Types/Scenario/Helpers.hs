module Arkham.Types.Scenario.Helpers
  ( module Arkham.Types.Scenario.Helpers
  , module X
  )
where

import Arkham.Prelude

import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Classes


import Arkham.Types.CampaignLogKey
import Arkham.Types.EncounterSet
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
  :: MonadRandom m => [CardCode] -> [EncounterSet] -> m [EncounterCard]
buildEncounterDeckExcluding cardCodes =
  buildEncounterDeckWith (filter ((`notElem` cardCodes) . ecCardCode))

buildEncounterDeckWith
  :: MonadRandom m
  => ([EncounterCard] -> [EncounterCard])
  -> [EncounterSet]
  -> m [EncounterCard]
buildEncounterDeckWith f encounterSets =
  shuffleM . f . concat =<< traverse gatherEncounterSet encounterSets
