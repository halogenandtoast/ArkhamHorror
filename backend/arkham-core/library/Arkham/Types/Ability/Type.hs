module Arkham.Types.Ability.Type where

import Arkham.Prelude

import Arkham.Types.Action
import Arkham.Types.Cost
import Arkham.Types.Modifier

data AbilityType
  = FastAbility Cost
  | ReactionAbility Cost
  | ActionAbility (Maybe Action) Cost
  | ForcedAbility
  | AbilityEffect Cost
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

applyAbilityTypeModifiers :: AbilityType -> [ModifierType] -> AbilityType
applyAbilityTypeModifiers aType modifiers = case aType of
  FastAbility cost -> FastAbility $ applyCostModifiers cost modifiers
  ReactionAbility cost -> ReactionAbility $ applyCostModifiers cost modifiers
  ActionAbility mAction cost ->
    ActionAbility mAction $ applyCostModifiers cost modifiers
  ForcedAbility -> ForcedAbility
  AbilityEffect cost -> AbilityEffect cost -- modifiers don't yet apply here

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
