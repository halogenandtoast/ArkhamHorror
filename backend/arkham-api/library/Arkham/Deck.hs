{-# LANGUAGE TemplateHaskell #-}

module Arkham.Deck where

import Arkham.Id
import Arkham.Investigator.Deck
import Arkham.Investigator.Deck qualified as Key
import Arkham.Prelude
import Arkham.Scenario.Deck
import Data.Aeson.TH
import GHC.OverloadedLabels

deckSignifierToScenarioDeckKey :: DeckSignifier -> Maybe ScenarioDeckKey
deckSignifierToScenarioDeckKey (ScenarioDeckByKey key) = Just key
deckSignifierToScenarioDeckKey _ = Nothing

data DeckSignifier
  = InvestigatorDeck InvestigatorId
  | InvestigatorDiscard InvestigatorId
  | EncounterDeck
  | EncounterDiscard
  | ScenarioDeckByKey ScenarioDeckKey
  | InvestigatorDeckByKey InvestigatorId InvestigatorDeckKey
  | EncounterDeckByKey ScenarioEncounterDeckKey
  | NoDeck
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "none" DeckSignifier where
  fromLabel = NoDeck

instance IsLabel "encounterDeck" DeckSignifier where
  fromLabel = EncounterDeck

instance IsLabel "encounterDiscard" DeckSignifier where
  fromLabel = EncounterDiscard

class IsDeck a where
  toDeck :: a -> DeckSignifier

instance IsDeck InvestigatorId where
  toDeck = InvestigatorDeck
  {-# INLINE toDeck #-}

instance IsDeck ScenarioDeckKey where
  toDeck = ScenarioDeckByKey
  {-# INLINE toDeck #-}

instance IsDeck ScenarioEncounterDeckKey where
  toDeck = EncounterDeckByKey
  {-# INLINE toDeck #-}

instance IsDeck DeckSignifier where
  toDeck = id
  {-# INLINE toDeck #-}

instance IsDeck (Maybe DeckSignifier) where
  toDeck = \case
    Nothing -> NoDeck
    Just ds -> ds
  {-# INLINE toDeck #-}

pattern HunchDeck :: InvestigatorId -> DeckSignifier
pattern HunchDeck iid <- InvestigatorDeckByKey iid Key.HunchDeck
  where
    HunchDeck iid = InvestigatorDeckByKey iid Key.HunchDeck

$(deriveJSON defaultOptions ''DeckSignifier)
