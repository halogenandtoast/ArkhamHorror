module Arkham.Types.Scenario.Deck where

import Arkham.Prelude

import Arkham.Types.Card

data ScenarioDeck = CultistDeck [Card] | ExhibitDeck [Card] | PotentialSacrifices [Card]
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)
