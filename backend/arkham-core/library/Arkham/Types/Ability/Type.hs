module Arkham.Types.Ability.Type where

import Arkham.Json
import Arkham.Types.Action hiding (Ability)
import Arkham.Types.Window
import ClassyPrelude

data AbilityType
  = FastAbility Window
  | ReactionAbility Window
  | ActionAbility Int (Maybe Action)
  | ForcedAbility
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

