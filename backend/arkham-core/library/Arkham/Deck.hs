module Arkham.Deck where

import Arkham.Prelude

import Arkham.Id
import Arkham.Scenario.Deck
import Arkham.Investigator.Deck

data DeckSignifier
  = InvestigatorDeck InvestigatorId
  | InvestigatorDiscard InvestigatorId
  | EncounterDeck
  | EncounterDiscard
  | ScenarioDeckByKey ScenarioDeckKey
  | InvestigatorDeckByKey InvestigatorId InvestigatorDeckKey
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
