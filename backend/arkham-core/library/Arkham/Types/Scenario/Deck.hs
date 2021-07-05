module Arkham.Types.Scenario.Deck where

import Arkham.Prelude

import Arkham.Types.Card

data ScenarioDeck = CultistDeck [EncounterCard] | ExhibitDeck [EncounterCard] | PotentialSacrifices [Card]
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)
