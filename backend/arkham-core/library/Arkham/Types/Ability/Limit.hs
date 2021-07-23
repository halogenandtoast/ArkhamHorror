module Arkham.Types.Ability.Limit where

import Arkham.Prelude

import Arkham.Types.InvestigatorId
import Arkham.Types.Trait

data AbilityLimit = PerInvestigatorLimit InvestigatorId AbilityLimitType Int | PlayerLimit AbilityLimitType Int | GroupLimit AbilityLimitType Int | NoLimit
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

abilityLimitType :: AbilityLimit -> Maybe AbilityLimitType
abilityLimitType (PerInvestigatorLimit _ t _) = Just t
abilityLimitType (PlayerLimit t _) = Just t
abilityLimitType (GroupLimit t _) = Just t
abilityLimitType NoLimit = Nothing

abilityLimitAmount :: AbilityLimit -> Maybe Int
abilityLimitAmount (PerInvestigatorLimit _ _ n) = Just n
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
  | PerSearch (Maybe Trait)
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)
