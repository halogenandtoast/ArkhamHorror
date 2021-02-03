module Arkham.Types.Scenario.Deck where

import Arkham.Import

data ScenarioDeck = CultistDeck [EncounterCard] | ExhibitDeck [LocationId] | PotentialSacrifices [Card]
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)
