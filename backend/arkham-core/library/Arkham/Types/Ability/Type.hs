module Arkham.Types.Ability.Type where

import Arkham.Prelude

import Arkham.Types.Action
import Arkham.Types.Cost
import Arkham.Types.Modifier
import Arkham.Types.WindowMatcher

data AbilityType
  = FreeAbility (Maybe WindowMatcher) Cost
  | ReactionAbility WindowMatcher Cost
  | ActionAbility (Maybe Action) Cost
  | ForcedAbility
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

applyAbilityTypeModifiers :: AbilityType -> [Modifier] -> AbilityType
applyAbilityTypeModifiers aType modifiers = case aType of
  FreeAbility mwindow cost ->
    FreeAbility mwindow $ applyCostModifiers cost modifiers
  ReactionAbility window cost ->
    ReactionAbility window (applyCostModifiers cost modifiers)
  ActionAbility mAction cost ->
    ActionAbility mAction $ applyCostModifiers cost modifiers
  ForcedAbility -> ForcedAbility

applyCostModifiers :: Cost -> [Modifier] -> Cost
applyCostModifiers = foldl' applyCostModifier

applyCostModifier :: Cost -> Modifier -> Cost
applyCostModifier (ActionCost n) (Modifier _ (ActionCostModifier m)) =
  ActionCost (max 0 $ n + m)
applyCostModifier (Costs (x : xs)) modifier@(Modifier _ (ActionCostModifier _))
  = case x of
    ActionCost _ -> Costs (applyCostModifier x modifier : xs)
    other -> other <> applyCostModifier (Costs xs) modifier
applyCostModifier (ActionCost _) (Modifier _ (ActionCostSetToModifier m)) =
  ActionCost m
applyCostModifier (Costs (x : xs)) modifier@(Modifier _ (ActionCostSetToModifier _))
  = case x of
    ActionCost _ -> Costs (applyCostModifier x modifier : xs)
    other -> other <> applyCostModifier (Costs xs) modifier
applyCostModifier cost _ = cost
