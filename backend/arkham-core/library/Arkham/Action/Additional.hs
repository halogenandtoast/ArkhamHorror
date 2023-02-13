module Arkham.Action.Additional where

import Arkham.Prelude

import Arkham.Action
import Arkham.Trait

data ActionRestriction = AbilitiesOnly | NoRestriction
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data AdditionalAction
  = TraitRestrictedAdditionalAction Trait ActionRestriction
  | ActionRestrictedAdditionalAction Action
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

