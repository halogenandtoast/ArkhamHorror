module Arkham.Types.Scenario.Deck where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.LocationId

data ScenarioDeck = CultistDeck [EncounterCard] | ExhibitDeck [LocationId] | PotentialSacrifices [Card]
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)
