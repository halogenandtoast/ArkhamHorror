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

evadeAction_ :: AbilityType
evadeAction_ = ActionAbility [Evade] $ ActionCost 1

instance IsLabel "evade" AbilityType where
  fromLabel = evadeAction_

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

doubleActionAbility :: AbilityType
doubleActionAbility = ActionAbility [] (ActionCost 2)

instance IsLabel "action" AbilityType where
  fromLabel = actionAbility

actionAbilityWithCost :: Cost -> AbilityType
actionAbilityWithCost cost = ActionAbility [] (ActionCost 1 <> cost)

freeReaction :: WindowMatcher -> AbilityType
freeReaction window = ReactionAbility window Free

triggered :: WindowMatcher -> Cost -> AbilityType
triggered = ReactionAbility

forced :: WindowMatcher -> AbilityType
forced = ForcedAbility

delayed :: AbilityType -> AbilityType
delayed = DelayedAbility

silent :: WindowMatcher -> AbilityType
silent = SilentForcedAbility

class HasCost c where
  overCost :: (Cost -> Cost) -> c -> c

pattern FastAbility :: Cost -> AbilityType
pattern FastAbility cost <- FastAbility' cost []
  where
    FastAbility cost = FastAbility' cost []

data AbilityType
  = FastAbility' {cost :: Cost, actions :: [Action]}
  | ReactionAbility {window :: WindowMatcher, cost :: Cost}
  | ConstantReaction {label :: Text, window :: WindowMatcher, cost :: Cost}
  | CustomizationReaction {label :: Text, window :: WindowMatcher, cost :: Cost}
  | ActionAbility {actions :: [Action], cost :: Cost}
  | ServitorAbility {action :: Action}
  | ActionAbilityWithSkill {actions :: [Action], skillType :: SkillType, cost :: Cost}
  | SilentForcedAbility {window :: WindowMatcher}
  | ForcedAbility {window :: WindowMatcher}
  | DelayedAbility {abilityType :: AbilityType}
  | ForcedAbilityWithCost {window :: WindowMatcher, cost :: Cost}
  | AbilityEffect {actions :: [Action], cost :: Cost}
  | Objective {abilityType :: AbilityType}
  | Haunted
  | Cosmos
  | ForcedWhen {criteria :: Criterion, abilityType :: AbilityType}
  deriving stock (Show, Ord, Eq, Data)

instance HasCost AbilityType where
  overCost f = \case
    FastAbility' cost actions -> FastAbility' (f cost) actions
    ReactionAbility window cost -> ReactionAbility window (f cost)
    CustomizationReaction label window cost -> CustomizationReaction label window (f cost)
    ConstantReaction label window cost -> ConstantReaction label window (f cost)
    ActionAbility actions cost -> ActionAbility actions (f cost)
    ServitorAbility action -> ServitorAbility action
    ActionAbilityWithSkill actions skillType cost ->
      ActionAbilityWithSkill actions skillType (f cost)
    SilentForcedAbility window -> SilentForcedAbility window
    ForcedAbility window -> ForcedAbility window
    ForcedAbilityWithCost window cost -> ForcedAbilityWithCost window (f cost)
    AbilityEffect as cost -> AbilityEffect as (f cost)
    Objective abilityType -> Objective (overCost f abilityType)
    DelayedAbility abilityType -> DelayedAbility (overCost f abilityType)
    Haunted -> Haunted
    Cosmos -> Cosmos
    ForcedWhen criteria abilityType -> ForcedWhen criteria (overCost f abilityType)

pattern Anytime :: AbilityType
pattern Anytime <- SilentForcedAbility AnyWindow
  where
    Anytime = SilentForcedAbility AnyWindow

abilityTypeCostL :: Traversal' AbilityType Cost
abilityTypeCostL f = \case
  FastAbility' cost action -> (`FastAbility'` action) <$> f cost
  ReactionAbility window cost -> ReactionAbility window <$> f cost
  CustomizationReaction label window cost -> CustomizationReaction label window <$> f cost
  ConstantReaction label window cost -> ConstantReaction label window <$> f cost
  ActionAbility action cost -> ActionAbility action <$> f cost
  ActionAbilityWithSkill action skillType cost ->
    ActionAbilityWithSkill action skillType <$> f cost
  SilentForcedAbility window -> SilentForcedAbility window <$ f mempty
  ForcedAbility window -> ForcedAbility window <$ f mempty
  ForcedAbilityWithCost window cost -> ForcedAbilityWithCost window <$> f cost
  AbilityEffect as cost -> AbilityEffect as <$> f cost
  Objective abilityType -> Objective <$> abilityTypeCostL f abilityType
  DelayedAbility abilityType -> DelayedAbility <$> abilityTypeCostL f abilityType
  ServitorAbility action -> pure $ ServitorAbility action
  Haunted -> pure Haunted
  Cosmos -> pure Cosmos
  ForcedWhen criteria abilityType ->
    ForcedWhen criteria <$> abilityTypeCostL f abilityType

$(deriveToJSON defaultOptions ''AbilityType)

instance FromJSON AbilityType where
  parseJSON = withObject "AbilityType" $ \o -> do
    tag :: Text <- o .: "tag"
    case tag of
      "ActionAbilityWithBefore" -> do
        a <- o .: "actions"
        c <- o .: "cost"
        pure $ ActionAbility {actions = a, cost = c}
      _ -> $(mkParseJSON defaultOptions ''AbilityType) (Object o)
