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

fightAction :: Cost -> AbilityType
fightAction cost = ActionAbility (Just Fight) (ActionCost 1 <> cost)

fightAction_ :: AbilityType
fightAction_ = fightAction mempty

instance IsLabel "fight" AbilityType where
  fromLabel = fightAction_

parleyAction :: Cost -> AbilityType
parleyAction cost = ActionAbility (Just Parley) (ActionCost 1 <> cost)

parleyAction_ :: AbilityType
parleyAction_ = parleyAction mempty

instance IsLabel "parley" AbilityType where
  fromLabel = parleyAction_

investigateAction :: Cost -> AbilityType
investigateAction cost = ActionAbility (Just Investigate) (ActionCost 1 <> cost)

actionAbility :: AbilityType
actionAbility = ActionAbility Nothing (ActionCost 1)

instance IsLabel "action" AbilityType where
  fromLabel = actionAbility

actionAbilityWithCost :: Cost -> AbilityType
actionAbilityWithCost cost = ActionAbility Nothing (ActionCost 1 <> cost)

freeReaction :: WindowMatcher -> AbilityType
freeReaction window = ReactionAbility window Free

pattern FastAbility :: Cost -> AbilityType
pattern FastAbility cost <- FastAbility' cost Nothing
  where
    FastAbility cost = FastAbility' cost Nothing

data AbilityType
  = FastAbility' {cost :: Cost, action :: Maybe Action}
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
