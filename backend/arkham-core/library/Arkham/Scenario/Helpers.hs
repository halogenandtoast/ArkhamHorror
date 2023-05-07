module Arkham.Scenario.Helpers (
  module Arkham.Scenario.Helpers,
  module X,
) where

import Arkham.Prelude

import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.EncounterSet
import Arkham.Game.Helpers as X
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Matcher

getHasRecordOrStandalone
  :: (HasGame m)
  => CampaignLogKey
  -> Bool
  -> m Bool
getHasRecordOrStandalone key def = do
  standalone <- selectNone TheCampaign
  if standalone then pure def else getHasRecord key

buildEncounterDeck :: (CardGen m) => [EncounterSet] -> m (Deck EncounterCard)
buildEncounterDeck = buildEncounterDeckWith id

buildEncounterDeckExcluding
  :: (CardGen m) => [CardDef] -> [EncounterSet] -> m (Deck EncounterCard)
buildEncounterDeckExcluding defs =
  buildEncounterDeckWith (filter ((`notElem` defs) . toCardDef))

buildEncounterDeckExcludingMatching
  :: (CardGen m)
  => CardMatcher
  -> [EncounterSet]
  -> m (Deck EncounterCard)
buildEncounterDeckExcludingMatching matcher =
  buildEncounterDeckWith (filter (not . (`cardMatch` matcher)))

excludeDoubleSided :: [EncounterCard] -> [EncounterCard]
excludeDoubleSided = filter (not . cdDoubleSided . toCardDef)

excludeBSides :: [EncounterCard] -> [EncounterCard]
excludeBSides = filter (not . isSuffixOf "b" . unCardCode . toCardCode)

buildEncounterDeckWith
  :: (CardGen m)
  => ([EncounterCard] -> [EncounterCard])
  -> [EncounterSet]
  -> m (Deck EncounterCard)
buildEncounterDeckWith f encounterSets =
  Deck
    <$> ( shuffleM
            . f
            . excludeBSides
            . excludeDoubleSided
            . concat
            =<< traverse gatherEncounterSet encounterSets
        )
