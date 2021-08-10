module Arkham.Types.Ability.Type where

import Arkham.Prelude

import Arkham.Types.Action
import Arkham.Types.Cost
import Arkham.Types.Matcher
import Arkham.Types.Modifier
import Arkham.Types.SkillType

data AbilityType
  = FastAbility Cost
  | ReactionAbility WindowMatcher Cost
  | ActionAbility (Maybe Action) Cost
  | ActionAbilityWithSkill (Maybe Action) SkillType Cost
  | ActionAbilityWithBefore (Maybe Action) (Maybe Action) Cost -- Action is first type, before is second
  | ForcedAbility WindowMatcher
  | AbilityEffect Cost
  | Objective AbilityType
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

abilityTypeAction :: AbilityType -> Maybe Action
abilityTypeAction = \case
  FastAbility _ -> Nothing
  ReactionAbility{} -> Nothing
  ActionAbility mAction _ -> mAction
  ActionAbilityWithSkill mAction _ _ -> mAction
  ActionAbilityWithBefore mAction _ _ -> mAction
  ForcedAbility _ -> Nothing
  AbilityEffect _ -> Nothing
  Objective aType -> abilityTypeAction aType

abilityTypeCost :: AbilityType -> Cost
abilityTypeCost = \case
  FastAbility cost -> cost
  ReactionAbility _ cost -> cost
  ActionAbility _ cost -> cost
  ActionAbilityWithSkill _ _ cost -> cost
  ActionAbilityWithBefore _ _ cost -> cost
  ForcedAbility _ -> Free
  AbilityEffect cost -> cost
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
  AbilityEffect cost -> AbilityEffect cost -- modifiers don't yet apply here
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
