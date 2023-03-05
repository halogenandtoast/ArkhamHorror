{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
module Arkham.Ability.Type where

import Arkham.Prelude

import Arkham.Ability.Limit
import Arkham.Action
import Arkham.Cost
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillType

data AbilityType
  = FastAbility { cost :: Cost }
  | ReactionAbility { window :: WindowMatcher, cost :: Cost }
  | ActionAbility { action :: Maybe Action, cost :: Cost }
  | ActionAbilityWithSkill { action :: Maybe Action, skillType :: SkillType, cost ::  Cost }
  | ActionAbilityWithBefore { action :: Maybe Action, actionBefore :: Maybe Action, cost :: Cost } -- Action is first type, before is second
  | SilentForcedAbility { window :: WindowMatcher }
  | ForcedAbility { window :: WindowMatcher }
  | ForcedAbilityWithCost { window :: WindowMatcher, cost :: Cost }
  | AbilityEffect { cost :: Cost }
  | Objective { abilityType :: AbilityType }
  | Haunted
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

abilityTypeAction :: AbilityType -> Maybe Action
abilityTypeAction = \case
  FastAbility _ -> Nothing
  ReactionAbility{} -> Nothing
  ActionAbility mAction _ -> mAction
  ActionAbilityWithSkill mAction _ _ -> mAction
  ActionAbilityWithBefore mAction _ _ -> mAction
  ForcedAbility _ -> Nothing
  SilentForcedAbility _ -> Nothing
  ForcedAbilityWithCost _ _ -> Nothing
  AbilityEffect _ -> Nothing
  Haunted -> Nothing
  Objective aType -> abilityTypeAction aType

abilityTypeCost :: AbilityType -> Cost
abilityTypeCost = \case
  FastAbility cost -> cost
  ReactionAbility _ cost -> cost
  ActionAbility _ cost -> cost
  ActionAbilityWithSkill _ _ cost -> cost
  ActionAbilityWithBefore _ _ cost -> cost
  SilentForcedAbility _ -> Free
  ForcedAbility _ -> Free
  ForcedAbilityWithCost _ cost -> cost
  AbilityEffect cost -> cost
  Haunted -> Free
  Objective aType -> abilityTypeCost aType

applyAbilityTypeModifiers :: AbilityType -> [ModifierType] -> AbilityType
applyAbilityTypeModifiers aType modifiers = case aType of
  FastAbility cost -> FastAbility $ applyCostModifiers cost modifiers
  ReactionAbility window cost ->
    ReactionAbility window $ applyCostModifiers cost modifiers
  ActionAbility mAction cost ->
    ActionAbility mAction $ applyCostModifiers cost modifiers
  ActionAbilityWithSkill mAction skill cost ->
    ActionAbilityWithSkill mAction skill $ applyCostModifiers cost modifiers
  ActionAbilityWithBefore mAction mBeforeAction cost ->
    ActionAbilityWithBefore mAction mBeforeAction
      $ applyCostModifiers cost modifiers
  ForcedAbility window -> ForcedAbility window
  SilentForcedAbility window -> SilentForcedAbility window
  ForcedAbilityWithCost window cost ->
    ForcedAbilityWithCost window $ applyCostModifiers cost modifiers
  AbilityEffect cost -> AbilityEffect cost -- modifiers don't yet apply here
  Haunted -> Haunted
  Objective aType' -> Objective $ applyAbilityTypeModifiers aType' modifiers

applyCostModifiers :: Cost -> [ModifierType] -> Cost
applyCostModifiers = foldl' applyCostModifier

applyCostModifier :: Cost -> ModifierType -> Cost
applyCostModifier (ActionCost n) (ActionCostModifier m) =
  ActionCost (max 0 $ n + m)
applyCostModifier (Costs (x : xs)) modifier@(ActionCostModifier _) = case x of
  ActionCost _ -> Costs (applyCostModifier x modifier : xs)
  other -> other <> applyCostModifier (Costs xs) modifier
applyCostModifier (ActionCost _) (ActionCostSetToModifier m) = ActionCost m
applyCostModifier (Costs (x : xs)) modifier@(ActionCostSetToModifier _) =
  case x of
    ActionCost _ -> Costs (applyCostModifier x modifier : xs)
    other -> other <> applyCostModifier (Costs xs) modifier
applyCostModifier cost _ = cost

defaultAbilityWindow :: AbilityType -> WindowMatcher
defaultAbilityWindow = \case
  FastAbility _ -> FastPlayerWindow
  ActionAbility{} -> DuringTurn You
  ActionAbilityWithBefore{} -> DuringTurn You
  ActionAbilityWithSkill{} -> DuringTurn You
  ForcedAbility window -> window
  SilentForcedAbility window -> window
  ForcedAbilityWithCost window _ -> window
  ReactionAbility window _ -> window
  AbilityEffect _ -> AnyWindow
  Haunted -> AnyWindow
  Objective aType -> defaultAbilityWindow aType

isFastAbilityType :: AbilityType -> Bool
isFastAbilityType = \case
  FastAbility{} -> True
  ForcedAbility{} -> False
  SilentForcedAbility{} -> False
  ForcedAbilityWithCost{} -> False
  Objective aType -> isFastAbilityType aType
  ReactionAbility{} -> False
  ActionAbility{} -> False
  ActionAbilityWithSkill{} -> False
  ActionAbilityWithBefore{} -> False
  AbilityEffect{} -> False
  Haunted{} -> False

isForcedAbilityType :: AbilityType -> Bool
isForcedAbilityType = \case
  SilentForcedAbility{} -> True
  ForcedAbility{} -> True
  ForcedAbilityWithCost{} -> True
  Objective aType -> isForcedAbilityType aType
  FastAbility{} -> False
  ReactionAbility{} -> False
  ActionAbility{} -> False
  ActionAbilityWithSkill{} -> False
  ActionAbilityWithBefore{} -> False
  AbilityEffect{} -> False
  Haunted{} -> True -- Maybe? we wanted this to basically never be valid but still take forced precedence

isReactionAbilityType :: AbilityType -> Bool
isReactionAbilityType = \case
  SilentForcedAbility{} -> False
  ForcedAbility{} -> False
  ForcedAbilityWithCost{} -> False
  Objective aType -> isReactionAbilityType aType
  FastAbility{} -> False
  ReactionAbility{} -> True
  ActionAbility{} -> False
  ActionAbilityWithSkill{} -> False
  ActionAbilityWithBefore{} -> False
  AbilityEffect{} -> False
  Haunted{} -> False

isSilentForcedAbilityType :: AbilityType -> Bool
isSilentForcedAbilityType = \case
  SilentForcedAbility{} -> True
  ForcedAbility{} -> False
  ForcedAbilityWithCost{} -> False
  Objective aType -> isSilentForcedAbilityType aType
  FastAbility{} -> False
  ReactionAbility{} -> False
  ActionAbility{} -> False
  ActionAbilityWithSkill{} -> False
  ActionAbilityWithBefore{} -> False
  AbilityEffect{} -> False
  Haunted{} -> False

isPerWindowLimit :: AbilityLimit -> Bool
isPerWindowLimit = \case
  GroupLimit l _ -> l == PerWindow
  PlayerLimit l _ -> l == PerWindow
  PerInvestigatorLimit l _ -> l == PerWindow
  PerCopyLimit _ l _ -> l == PerWindow
  NoLimit -> False

defaultAbilityLimit :: AbilityType -> AbilityLimit
defaultAbilityLimit = \case
  ForcedAbility _ -> GroupLimit PerWindow 1
  SilentForcedAbility _ -> GroupLimit PerWindow 1
  ForcedAbilityWithCost _ _ -> GroupLimit PerWindow 1
  ReactionAbility _ _ -> PlayerLimit PerWindow 1
  FastAbility _ -> NoLimit
  ActionAbility _ _ -> NoLimit
  ActionAbilityWithBefore{} -> NoLimit
  ActionAbilityWithSkill{} -> NoLimit
  AbilityEffect _ -> NoLimit
  Objective aType -> defaultAbilityLimit aType
  Haunted -> NoLimit
