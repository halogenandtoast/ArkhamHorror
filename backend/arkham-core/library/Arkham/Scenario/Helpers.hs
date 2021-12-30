module Arkham.Scenario.Helpers
  ( module Arkham.Scenario.Helpers
  , module X
  ) where

import Arkham.Prelude

import Arkham.EncounterSet
import Arkham.CampaignId
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Game.Helpers as X
import Arkham.Helpers

getHasRecordOrStandalone
  :: (MonadReader env m, HasRecord env (), HasId (Maybe CampaignId) env ())
  => CampaignLogKey
  -> Bool
  -> m Bool
getHasRecordOrStandalone key def = do
  standalone <- isNothing <$> getId @(Maybe CampaignId) ()
  if standalone then pure def else getHasRecord key

buildEncounterDeck :: MonadRandom m => [EncounterSet] -> m (Deck EncounterCard)
buildEncounterDeck = buildEncounterDeckWith id

buildEncounterDeckExcluding
  :: MonadRandom m => [CardDef] -> [EncounterSet] -> m (Deck EncounterCard)
buildEncounterDeckExcluding defs =
  buildEncounterDeckWith (filter ((`notElem` defs) . toCardDef))

excludeDoubleSided :: [EncounterCard] -> [EncounterCard]
excludeDoubleSided = filter (not . cdDoubleSided . toCardDef)

excludeBSides :: [EncounterCard] -> [EncounterCard]
excludeBSides = filter (not . isSuffixOf "b" . unCardCode . toCardCode)

buildEncounterDeckWith
  :: MonadRandom m
  => ([EncounterCard] -> [EncounterCard])
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
