{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Ability.Type where

import Arkham.Prelude

import Arkham.Action
import Arkham.Cost
import Arkham.Criteria (Criterion)
import Arkham.Matcher
import Arkham.SkillType
import Data.Aeson.TH
import GHC.OverloadedLabels

evadeAction :: Cost -> AbilityType
evadeAction cost = ActionAbility [Evade] (ActionCost 1 <> cost)

fightAction :: Cost -> AbilityType
fightAction cost = ActionAbility [Fight] (ActionCost 1 <> cost)

fightAction_ :: AbilityType
fightAction_ = fightAction mempty

instance IsLabel "fight" AbilityType where
  fromLabel = fightAction_

parleyAction :: Cost -> AbilityType
parleyAction cost = ActionAbility [Parley] (ActionCost 1 <> cost)

parleyAction_ :: AbilityType
parleyAction_ = parleyAction mempty

instance IsLabel "parley" AbilityType where
  fromLabel = parleyAction_

investigateAction :: Cost -> AbilityType
investigateAction cost = ActionAbility [Investigate] (ActionCost 1 <> cost)

investigateAction_ :: AbilityType
investigateAction_ = investigateAction mempty

actionAbility :: AbilityType
actionAbility = ActionAbility [] (ActionCost 1)

instance IsLabel "action" AbilityType where
  fromLabel = actionAbility

actionAbilityWithCost :: Cost -> AbilityType
actionAbilityWithCost cost = ActionAbility [] (ActionCost 1 <> cost)

freeReaction :: WindowMatcher -> AbilityType
freeReaction window = ReactionAbility window Free

pattern FastAbility :: Cost -> AbilityType
pattern FastAbility cost <- FastAbility' cost []
  where
    FastAbility cost = FastAbility' cost []

data AbilityType
  = FastAbility' {cost :: Cost, actions :: [Action]}
  | ReactionAbility {window :: WindowMatcher, cost :: Cost}
  | ActionAbility {actions :: [Action], cost :: Cost}
  | ActionAbilityWithSkill {actions :: [Action], skillType :: SkillType, cost :: Cost}
  | ActionAbilityWithBefore {actions :: [Action], actionBefore :: Action, cost :: Cost} -- Action is first type, before is second
  | SilentForcedAbility {window :: WindowMatcher}
  | ForcedAbility {window :: WindowMatcher}
  | ForcedAbilityWithCost {window :: WindowMatcher, cost :: Cost}
  | AbilityEffect {cost :: Cost}
  | Objective {abilityType :: AbilityType}
  | Haunted
  | Cosmos
  | ForcedWhen {criteria :: Criterion, abilityType :: AbilityType}
  deriving stock (Show, Ord, Eq, Data)

abilityTypeCostL :: Traversal' AbilityType Cost
abilityTypeCostL f = \case
  FastAbility' cost action -> (`FastAbility'` action) <$> f cost
  ReactionAbility window cost -> ReactionAbility window <$> f cost
  ActionAbility action cost -> ActionAbility action <$> f cost
  ActionAbilityWithSkill action skillType cost ->
    ActionAbilityWithSkill action skillType <$> f cost
  ActionAbilityWithBefore action actionBefore cost ->
    ActionAbilityWithBefore action actionBefore <$> f cost
  SilentForcedAbility window -> SilentForcedAbility window <$ f mempty
  ForcedAbility window -> ForcedAbility window <$ f mempty
  ForcedAbilityWithCost window cost -> ForcedAbilityWithCost window <$> f cost
  AbilityEffect cost -> AbilityEffect <$> f cost
  Objective abilityType -> Objective <$> abilityTypeCostL f abilityType
  Haunted -> pure Haunted
  Cosmos -> pure Cosmos
  ForcedWhen criteria abilityType ->
    ForcedWhen criteria <$> abilityTypeCostL f abilityType

$(deriveJSON defaultOptions ''AbilityType)
