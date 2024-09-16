module Arkham.EncounterCard.Source where

import Arkham.Prelude
import GHC.OverloadedLabels

data EncounterCardSource
  = FromDiscard
  | FromEncounterDeck
  | FromTheVoid
  | FromVictoryDisplay
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance IsLabel "discard" EncounterCardSource where
  fromLabel = FromDiscard

instance IsLabel "encounterDeck" EncounterCardSource where
  fromLabel = FromEncounterDeck
