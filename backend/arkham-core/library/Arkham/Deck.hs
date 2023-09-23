{-# LANGUAGE TemplateHaskell #-}

module Arkham.Deck where

import Arkham.Prelude

import Arkham.Id
import Arkham.Investigator.Deck
import Arkham.Investigator.Deck qualified as Key
import Arkham.Scenario.Deck
import Data.Aeson.TH

data DeckSignifier
  = InvestigatorDeck InvestigatorId
  | InvestigatorDiscard InvestigatorId
  | EncounterDeck
  | EncounterDiscard
  | ScenarioDeckByKey ScenarioDeckKey
  | InvestigatorDeckByKey InvestigatorId InvestigatorDeckKey
  | EncounterDeckByKey ScenarioEncounterDeckKey
  deriving stock (Show, Eq, Ord, Data)

class IsDeck a where
  toDeck :: a -> DeckSignifier

instance IsDeck InvestigatorId where
  toDeck = InvestigatorDeck
  {-# INLINE toDeck #-}

instance IsDeck ScenarioDeckKey where
  toDeck = ScenarioDeckByKey
  {-# INLINE toDeck #-}

instance IsDeck DeckSignifier where
  toDeck = id
  {-# INLINE toDeck #-}

pattern HunchDeck :: InvestigatorId -> DeckSignifier
pattern HunchDeck iid <- InvestigatorDeckByKey iid Key.HunchDeck
  where
    HunchDeck iid = InvestigatorDeckByKey iid Key.HunchDeck

$(deriveJSON defaultOptions ''DeckSignifier)
