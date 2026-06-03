{-# LANGUAGE TemplateHaskell #-}

module Arkham.Ability.Limit where

import Arkham.Prelude

import Arkham.Card.CardDef
import Arkham.Id
import Arkham.Trait
import Data.Aeson.TH

data CanIgnoreAbilityLimit = CanIgnoreAbilityLimit | CanNotIgnoreAbilityLimit
  deriving stock (Eq, Data)

data AbilityLimit
  = PerInvestigatorLimit AbilityLimitType Int
  | PlayerLimit AbilityLimitType Int
  | GroupLimit AbilityLimitType Int
  | MaxPer CardDef AbilityLimitType Int
  | NoLimit
  deriving stock (Show, Eq, Ord, Data)

abilityLimitType :: AbilityLimit -> Maybe AbilityLimitType
abilityLimitType (PerInvestigatorLimit t _) = Just t
abilityLimitType (PlayerLimit t _) = Just t
abilityLimitType (GroupLimit t _) = Just t
abilityLimitType (MaxPer _ t _) = Just t
abilityLimitType NoLimit = Nothing
data AbilityLimitType
  = PerGame
  | PerPhase
  | PerRound
  | PerTurn
  | PerWindow
  | PerAttack
  | PerMove -- DEPRECATED
  | PerMovement MovementId
  | PerSpawn
  | PerTest
  | PerTestOrAbility
  | PerSearch Trait
  | PerDepthLevel
  | PerCampaign
  | SuccessPerGame
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''AbilityLimitType)
$(deriveJSON defaultOptions ''AbilityLimit)
