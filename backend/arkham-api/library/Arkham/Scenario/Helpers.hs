module Arkham.Scenario.Helpers (
  module Arkham.Scenario.Helpers,
  module X,
) where

import Arkham.Prelude

import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.EncounterSet
import Arkham.Helpers
import Arkham.Helpers.Campaign as X
import Arkham.Helpers.EncounterSet (gatherEncounterSet)
import Arkham.Helpers.Log (getHasRecord)
import Arkham.Matcher
import Arkham.Tracing

getHasRecordOrStandalone
  :: (HasGame m, Tracing m, IsCampaignLogKey k)
  => k
  -> Bool
  -> m Bool
getHasRecordOrStandalone key def = do
  standalone <- selectNone TheCampaign
  if standalone then pure def else getHasRecord key

buildEncounterDeck :: CardGen m => [EncounterSet] -> m (Deck EncounterCard)
buildEncounterDeck = buildEncounterDeckWith id

buildEncounterDeckExcluding
  :: CardGen m => [CardDef] -> [EncounterSet] -> m (Deck EncounterCard)
buildEncounterDeckExcluding defs =
  buildEncounterDeckWith (filter ((`notElem` defs) . toCardDef))

buildEncounterDeckExcludingMatching
  :: CardGen m
  => CardMatcher
  -> [EncounterSet]
  -> m (Deck EncounterCard)
buildEncounterDeckExcludingMatching matcher =
  buildEncounterDeckWith (filter (not . (`cardMatch` matcher)))

excludeDoubleSided :: [EncounterCard] -> [EncounterCard]
excludeDoubleSided = filter (not . isDoubleSided)

excludeBSides :: [EncounterCard] -> [EncounterCard]
excludeBSides = filter (not . hasBSide)

hasBSide :: EncounterCard -> Bool
hasBSide = and . sequence [isDoubleSided, isSuffixOf "b" . unCardCode . toCardCode]

-- Location cards are weird because they are always double sided, but when we
-- gather cards we want to include them even when the suffix is a "b", in these
-- cases we just look to ensure it's not the other side of a card.
isDoubleSided :: EncounterCard -> Bool
isDoubleSided ec = if cdCardType def == LocationType
  then cdDoubleSided def
  else isJust (cdOtherSide def)
 where
   def = toCardDef ec

buildEncounterDeckWith
  :: CardGen m
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
