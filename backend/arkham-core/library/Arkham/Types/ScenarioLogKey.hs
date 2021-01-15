module Arkham.Types.ScenarioLogKey where

import Arkham.Prelude

import Arkham.Types.InvestigatorId

data ScenarioLogKey
  = HadADrink InvestigatorId
  | Cheated InvestigatorId
  | FoundAStrangeDoll
  | FoundAnAncientBindingStone
  | StolenAPassengersLuggage
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, Hashable, FromJSONKey)
