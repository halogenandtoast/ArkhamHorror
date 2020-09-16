module Arkham.Types.Ability.Limit where

import Arkham.Json
import ClassyPrelude

data AbilityLimit
  = NoLimit
  | PerGame
  | PerPhase
  | PerRound
  | PerTurn
  | PerTestOrAbility
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

