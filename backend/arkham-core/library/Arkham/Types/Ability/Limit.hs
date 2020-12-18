module Arkham.Types.Ability.Limit where

import Arkham.Prelude

data AbilityLimit = PlayerLimit AbilityLimitType Int | GroupLimit AbilityLimitType Int | NoLimit
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

abilityLimitType :: AbilityLimit -> Maybe AbilityLimitType
abilityLimitType (PlayerLimit t _) = Just t
abilityLimitType (GroupLimit t _) = Just t
abilityLimitType NoLimit = Nothing

abilityLimitAmount :: AbilityLimit -> Maybe Int
abilityLimitAmount (PlayerLimit _ n) = Just n
abilityLimitAmount (GroupLimit _ n) = Just n
abilityLimitAmount NoLimit = Nothing

data AbilityLimitType
  = PerGame
  | PerPhase
  | PerRound
  | PerTurn
  | PerWindow
  | PerTestOrAbility
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)
