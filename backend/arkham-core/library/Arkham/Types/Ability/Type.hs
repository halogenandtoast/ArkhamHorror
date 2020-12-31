module Arkham.Types.Ability.Type where

import Arkham.Prelude

import Arkham.Types.Action
import Arkham.Types.Cost
import Arkham.Types.Window

data AbilityType
  = FastAbility Window
  | ReactionAbility Window
  | ActionAbility (Maybe Action) Cost
  | ForcedAbility
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

