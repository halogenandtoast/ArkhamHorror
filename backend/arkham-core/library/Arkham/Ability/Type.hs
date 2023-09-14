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

fightAction :: Cost -> AbilityType
fightAction cost = ActionAbility (Just Fight) (ActionCost 1 <> cost)

fightAction_ :: AbilityType
fightAction_ = ActionAbility (Just Fight) (ActionCost 1)

actionAbility :: AbilityType
actionAbility = ActionAbility Nothing (ActionCost 1)

actionAbilityWithCost :: Cost -> AbilityType
actionAbilityWithCost cost = ActionAbility Nothing (ActionCost 1 <> cost)

freeReaction :: WindowMatcher -> AbilityType
freeReaction window = ReactionAbility window Free

data AbilityType
  = FastAbility {cost :: Cost}
  | ReactionAbility {window :: WindowMatcher, cost :: Cost}
  | ActionAbility {action :: Maybe Action, cost :: Cost}
  | ActionAbilityWithSkill {action :: Maybe Action, skillType :: SkillType, cost :: Cost}
  | ActionAbilityWithBefore {action :: Maybe Action, actionBefore :: Maybe Action, cost :: Cost} -- Action is first type, before is second
  | SilentForcedAbility {window :: WindowMatcher}
  | ForcedAbility {window :: WindowMatcher}
  | ForcedAbilityWithCost {window :: WindowMatcher, cost :: Cost}
  | AbilityEffect {cost :: Cost}
  | Objective {abilityType :: AbilityType}
  | Haunted
  | Cosmos
  | ForcedWhen {criteria :: Criterion, abilityType :: AbilityType}
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''AbilityType)
