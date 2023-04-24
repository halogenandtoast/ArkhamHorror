{-# LANGUAGE TemplateHaskell #-}
module Arkham.Deck where

import Arkham.Prelude

import Arkham.Id
import Arkham.Scenario.Deck
import Arkham.Investigator.Deck
import Data.Aeson.TH

data DeckSignifier
  = InvestigatorDeck InvestigatorId
  | InvestigatorDiscard InvestigatorId
  | EncounterDeck
  | EncounterDiscard
  | ScenarioDeckByKey ScenarioDeckKey
  | InvestigatorDeckByKey InvestigatorId InvestigatorDeckKey
  deriving stock (Show, Eq, Ord)

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

$(deriveJSON defaultOptions ''DeckSignifier)
