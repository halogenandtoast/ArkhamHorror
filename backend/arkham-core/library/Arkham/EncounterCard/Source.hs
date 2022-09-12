module Arkham.EncounterCard.Source where

import Arkham.Prelude

data EncounterCardSource
  = FromDiscard
  | FromEncounterDeck
  | FromTheVoid
  | FromVictoryDisplay
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
