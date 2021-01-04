module Arkham.Types.Ability.Type where

import Arkham.Prelude

import Arkham.Types.Action
import Arkham.Types.Cost

data AbilityType
  = FastAbility Cost
  | ReactionAbility Cost
  | ActionAbility (Maybe Action) Cost
  | ForcedAbility
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

