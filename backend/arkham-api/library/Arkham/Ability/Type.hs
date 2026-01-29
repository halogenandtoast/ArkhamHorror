{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Ability.Type where

import Arkham.Action
import Arkham.Cost
import Arkham.Criteria (Criterion)
import Arkham.Matcher
import Arkham.Prelude
import Arkham.SkillType
import Data.Aeson.TH
import GHC.OverloadedLabels
import GHC.Records

evadeAction :: Cost -> AbilityType
evadeAction cost = ActionAbility [Evade] #agility (ActionCost 1 <> cost)

evadeAction_ :: AbilityType
evadeAction_ = ActionAbility [Evade] #agility $ ActionCost 1

evadeActionWithAlternate :: AbilitySkills -> Cost -> AbilityType
evadeActionWithAlternate stype cost = ActionAbility [Evade] (Just $ OrAbilitySkills [#agility, stype]) (ActionCost 1 <> cost)

evadeActionWithAlternate_ :: AbilitySkills -> AbilityType
evadeActionWithAlternate_ stype = ActionAbility [Evade] (Just $ OrAbilitySkills [#agility, stype]) (ActionCost 1)

instance IsLabel "evade" AbilityType where
  fromLabel = evadeAction_

fightActionWith :: SkillType -> Cost -> AbilityType
fightActionWith stype cost = ActionAbility [Fight] (Just $ AbilitySkill stype) (ActionCost 1 <> cost)

fightActionWith_ :: SkillType -> AbilityType
fightActionWith_ stype = ActionAbility [Fight] (Just $ AbilitySkill stype) (ActionCost 1)

fightAction :: Cost -> AbilityType
fightAction cost = ActionAbility [Fight] #combat (ActionCost 1 <> cost)

fightActionWithAlternate :: AbilitySkills -> Cost -> AbilityType
fightActionWithAlternate stype cost = ActionAbility [Fight] (Just $ OrAbilitySkills [#combat, stype]) (ActionCost 1 <> cost)

fightActionWithAlternate_ :: AbilitySkills -> AbilityType
fightActionWithAlternate_ stype = ActionAbility [Fight] (Just $ OrAbilitySkills [#combat, stype]) (ActionCost 1)

fightAction_ :: AbilityType
fightAction_ = fightAction mempty

instance IsLabel "fight" AbilityType where
  fromLabel = fightAction_

parleyAction :: Cost -> AbilityType
parleyAction cost = ActionAbility [Parley] Nothing (ActionCost 1 <> cost)

parleyAction_ :: AbilityType
parleyAction_ = parleyAction mempty

instance IsLabel "parley" AbilityType where
  fromLabel = parleyAction_

investigateAction :: Cost -> AbilityType
investigateAction cost = ActionAbility [Investigate] #intellect (ActionCost 1 <> cost)

investigateAction_ :: AbilityType
investigateAction_ = investigateAction mempty

investigateActionWith :: SkillType -> Cost -> AbilityType
investigateActionWith stype cost = ActionAbility [Investigate] (Just $ AbilitySkill stype) (ActionCost 1 <> cost)

investigateActionWith_ :: SkillType -> AbilityType
investigateActionWith_ stype = ActionAbility [Investigate] (Just $ AbilitySkill stype) (ActionCost 1)

investigateActionWithAlternate :: AbilitySkills -> Cost -> AbilityType
investigateActionWithAlternate stype cost = ActionAbility [Investigate] (Just $ OrAbilitySkills [#intellect, stype]) (ActionCost 1 <> cost)

investigateActionWithAlternate_ :: AbilitySkills -> AbilityType
investigateActionWithAlternate_ stype = ActionAbility [Investigate] (Just $ OrAbilitySkills [#intellect, stype]) (ActionCost 1)

actionAbility :: AbilityType
actionAbility = ActionAbility [] Nothing (ActionCost 1)

doubleActionAbility :: AbilityType
doubleActionAbility = ActionAbility [] Nothing (ActionCost 2)

instance IsLabel "action" AbilityType where
  fromLabel = actionAbility

actionAbilityWithCost :: Cost -> AbilityType
actionAbilityWithCost cost = ActionAbility [] Nothing (ActionCost 1 <> cost)

freeReaction :: WindowMatcher -> AbilityType
freeReaction window = ReactionAbility window Free []

triggered :: WindowMatcher -> Cost -> AbilityType
triggered wm cost = ReactionAbility wm cost []

triggeredAction :: Action -> WindowMatcher -> Cost -> AbilityType
triggeredAction action wm cost = ReactionAbility wm cost [action]

triggered_ :: WindowMatcher -> AbilityType
triggered_ wm = ReactionAbility wm Free []

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

freeTrigger_ :: AbilityType
freeTrigger_ = FastAbility' Free []

freeTrigger :: Cost -> AbilityType
freeTrigger c = FastAbility' c []

data AbilitySkills
  = AbilitySkill SkillType
  | AndAbilitySkills [AbilitySkills]
  | OrAbilitySkills [AbilitySkills]
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "willpower" AbilitySkills where
  fromLabel = AbilitySkill #willpower

instance IsLabel "intellect" AbilitySkills where
  fromLabel = AbilitySkill #intellect

instance IsLabel "combat" AbilitySkills where
  fromLabel = AbilitySkill #combat

instance IsLabel "agility" AbilitySkills where
  fromLabel = AbilitySkill #agility

instance IsLabel "willpower" (Maybe AbilitySkills) where
  fromLabel = Just #willpower

instance IsLabel "intellect" (Maybe AbilitySkills) where
  fromLabel = Just #intellect

instance IsLabel "combat" (Maybe AbilitySkills) where
  fromLabel = Just #combat

instance IsLabel "agility" (Maybe AbilitySkills) where
  fromLabel = Just #agility

data AbilityType
  = FastAbility' {cost :: Cost, actions :: [Action]}
  | ReactionAbility {window :: WindowMatcher, cost :: Cost, actions :: [Action]}
  | ConstantReaction {label :: Text, window :: WindowMatcher, cost :: Cost}
  | CustomizationReaction {label :: Text, window :: WindowMatcher, cost :: Cost}
  | ActionAbility {actions :: [Action], skillTypes :: Maybe AbilitySkills, cost :: Cost}
  | ServitorAbility {action :: Action}
  | SilentForcedAbility {window :: WindowMatcher}
  | ForcedAbility {window :: WindowMatcher}
  | DelayedAbility {abilityType :: AbilityType}
  | ForcedAbilityWithCost {window :: WindowMatcher, cost :: Cost}
  | AbilityEffect {actions :: [Action], cost :: Cost}
  | Objective {abilityType :: AbilityType}
  | Haunted
  | Cosmos
  | ForcedWhen {criteria :: Criterion, abilityType :: AbilityType}
  | ConstantAbility
  deriving stock (Show, Ord, Eq, Data)

instance HasField "fast" AbilityType Bool where
  getField = isFastAbilityType

overAbilityTypeActions :: ([Action] -> [Action]) -> AbilityType -> AbilityType
overAbilityTypeActions f = \case
  FastAbility' cost actions -> FastAbility' cost (f actions)
  ActionAbility actions skillTypes cost -> ActionAbility (f actions) skillTypes cost
  AbilityEffect actions cost -> AbilityEffect (f actions) cost
  Objective abilityType -> Objective (overAbilityTypeActions f abilityType)
  DelayedAbility abilityType -> DelayedAbility (overAbilityTypeActions f abilityType)
  ForcedWhen criteria abilityType -> ForcedWhen criteria (overAbilityTypeActions f abilityType)
  ReactionAbility window cost actions -> ReactionAbility window cost (f actions)
  CustomizationReaction label window cost -> CustomizationReaction label window cost
  ConstantReaction label window cost -> ConstantReaction label window cost
  ServitorAbility action -> ServitorAbility action
  SilentForcedAbility window -> SilentForcedAbility window
  ForcedAbility window -> ForcedAbility window
  ForcedAbilityWithCost window cost -> ForcedAbilityWithCost window cost
  Haunted -> Haunted
  Cosmos -> Cosmos
  ConstantAbility -> ConstantAbility

instance HasCost AbilityType where
  overCost f = \case
    FastAbility' cost actions -> FastAbility' (f cost) actions
    ReactionAbility window cost actions -> ReactionAbility window (f cost) actions
    CustomizationReaction label window cost -> CustomizationReaction label window (f cost)
    ConstantReaction label window cost -> ConstantReaction label window (f cost)
    ActionAbility actions skillTypes cost -> ActionAbility actions skillTypes (f cost)
    ServitorAbility action -> ServitorAbility action
    SilentForcedAbility window -> SilentForcedAbility window
    ForcedAbility window -> ForcedAbility window
    ForcedAbilityWithCost window cost -> ForcedAbilityWithCost window (f cost)
    AbilityEffect as cost -> AbilityEffect as (f cost)
    Objective abilityType -> Objective (overCost f abilityType)
    DelayedAbility abilityType -> DelayedAbility (overCost f abilityType)
    Haunted -> Haunted
    Cosmos -> Cosmos
    ForcedWhen criteria abilityType -> ForcedWhen criteria (overCost f abilityType)
    ConstantAbility -> ConstantAbility

pattern Anytime :: AbilityType
pattern Anytime <- SilentForcedAbility AnyWindow
  where
    Anytime = SilentForcedAbility AnyWindow

isFastAbilityType :: AbilityType -> Bool
isFastAbilityType = \case
  FastAbility' {} -> True
  ForcedAbility {} -> False
  SilentForcedAbility {} -> False
  ForcedAbilityWithCost {} -> False
  Objective aType -> isFastAbilityType aType
  DelayedAbility aType -> isFastAbilityType aType
  ReactionAbility {} -> False
  CustomizationReaction {} -> False
  ConstantReaction {} -> False
  ActionAbility {} -> False
  AbilityEffect {} -> False
  Haunted {} -> False
  ServitorAbility {} -> False
  Cosmos {} -> False
  ForcedWhen _ aType -> isFastAbilityType aType
  ConstantAbility -> False

abilityTypeCostL :: Traversal' AbilityType Cost
abilityTypeCostL f = \case
  FastAbility' cost action -> (`FastAbility'` action) <$> f cost
  ReactionAbility window cost actions -> ReactionAbility window <$> f cost <*> pure actions
  CustomizationReaction label window cost -> CustomizationReaction label window <$> f cost
  ConstantReaction label window cost -> ConstantReaction label window <$> f cost
  ActionAbility action skillTypes cost -> ActionAbility action skillTypes <$> f cost
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
  ConstantAbility -> pure ConstantAbility

mconcat
  [ deriveToJSON defaultOptions ''AbilityType
  , deriveJSON defaultOptions ''AbilitySkills
  ]

instance FromJSON AbilityType where
  parseJSON = withObject "AbilityType" $ \o -> do
    tag :: Text <- o .: "tag"
    case tag of
      "ReactionAbility" -> do
        w <- o .: "window"
        c <- o .: "cost"
        a <- o .:? "actions" .!= []
        pure $ ReactionAbility {window = w, cost = c, actions = a}
      "ActionAbility" -> do
        actions <- o .: "actions"
        cost <- o .: "cost"
        skillTypes <- o .:? "skillTypes"
        pure $ ActionAbility {..}
      "ActionAbilityWithSkill" -> do
        actions <- o .: "actions"
        cost <- o .: "cost"
        skillType <- o .: "skillType"
        pure $ ActionAbility {actions, cost, skillTypes = Just (AbilitySkill skillType)}
      "ActionAbilityWithBefore" -> do
        actions <- o .: "actions"
        cost <- o .: "cost"
        pure $ ActionAbility {actions, cost, skillTypes = Nothing}
      _ -> $(mkParseJSON defaultOptions ''AbilityType) (Object o)
