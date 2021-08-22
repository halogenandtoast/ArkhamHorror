module Arkham.Types.Deck where

import Arkham.Prelude

import Arkham.Types.Id

data DeckSignifier
  = InvestigatorDeck InvestigatorId
  | EncounterDeck
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
