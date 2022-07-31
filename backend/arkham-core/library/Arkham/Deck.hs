module Arkham.Deck where

import Arkham.Prelude

import Arkham.Id

data DeckSignifier
  = InvestigatorDeck InvestigatorId
  | InvestigatorDiscard InvestigatorId
  | EncounterDeck
  | EncounterDiscard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
