module Arkham.Ability (
  module X,
  module Arkham.Ability,
) where

import Arkham.Prelude

import Arkham.Ability.Limit as X
import Arkham.Ability.Type as X hiding (abilityType)
import Arkham.Ability.Types as X
import Arkham.Ability.Used as X
import Arkham.Cost as X
import Arkham.Criteria as X

import Arkham.Action hiding (Ability)
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Modifier
import Arkham.Source
import Control.Lens (set)

inHandAbility :: Ability -> Bool
inHandAbility = inHandCriteria . abilityCriteria
 where
  inHandCriteria = \case
    InYourHand -> True
    Criteria xs -> any inHandCriteria xs
    AnyCriterion xs -> any inHandCriteria xs
    _ -> False

abilityCost :: Ability -> Cost
abilityCost = abilityTypeCost . abilityType

abilityAction :: Ability -> Maybe Action
abilityAction = abilityTypeAction . abilityType

abilityIs :: Ability -> Action -> Bool
abilityIs a = (== abilityAction a) . Just

abilityIsActionAbility :: Ability -> Bool
abilityIsActionAbility a = case abilityType a of
  ActionAbility {} -> True
  ActionAbilityWithSkill {} -> True
  ActionAbilityWithBefore {} -> True
  _ -> False

abilityIsFastAbility :: Ability -> Bool
abilityIsFastAbility a = case abilityType a of
  FastAbility {} -> True
  _ -> False

abilityIsForcedAbility :: Ability -> Bool
abilityIsForcedAbility a = case abilityType a of
  ForcedAbility {} -> True
  ForcedAbilityWithCost {} -> True
  _ -> False

abilityIsReactionAbility :: Ability -> Bool
abilityIsReactionAbility a = case abilityType a of
  ReactionAbility {} -> True
  _ -> False

doesNotProvokeAttacksOfOpportunity :: Ability -> Ability
doesNotProvokeAttacksOfOpportunity =
  set abilityDoesNotProvokeAttacksOfOpportunityL True

displayAsAction :: Ability -> Ability
displayAsAction = set abilityDisplayAsActionL True

limitedAbility :: AbilityLimit -> Ability -> Ability
limitedAbility l a = a & abilityLimitL .~ l

playerLimit :: AbilityLimitType -> Ability -> Ability
playerLimit lType = limitedAbility (PlayerLimit lType 1)

groupLimit :: AbilityLimitType -> Ability -> Ability
groupLimit lType = limitedAbility (GroupLimit lType 1)

withTooltip :: Text -> Ability -> Ability
withTooltip t a = a & abilityTooltipL ?~ t

restrictedAbility
  :: Sourceable a => a -> Int -> Criterion -> AbilityType -> Ability
restrictedAbility entity idx restriction type' =
  (mkAbility entity idx type') {abilityCriteria = restriction}

controlledAbility
  :: Sourceable a => a -> Int -> Criterion -> AbilityType -> Ability
controlledAbility entity idx restriction = restrictedAbility entity idx (ControlsThis <> restriction)

fastAbility :: Sourceable a => a -> Int -> Cost -> Criterion -> Ability
fastAbility entity idx cost criteria =
  (mkAbility entity idx (FastAbility cost))
    { abilityCriteria = criteria
    }

fightAbility :: Sourceable a => a -> Int -> Cost -> Criterion -> Ability
fightAbility entity idx cost criteria =
  (mkAbility entity idx (fightAction cost))
    { abilityCriteria = criteria
    }

evadeAbility :: Sourceable a => a -> Int -> Cost -> Criterion -> Ability
evadeAbility entity idx cost criteria =
  (mkAbility entity idx (ActionAbility (Just #evade) cost))
    { abilityCriteria = criteria
    }

investigateAbility :: Sourceable a => a -> Int -> Cost -> Criterion -> Ability
investigateAbility entity idx cost criteria =
  (mkAbility entity idx (investigateAction cost))
    { abilityCriteria = criteria
    }

reactionAbility
  :: Sourceable a
  => a
  -> Int
  -> Cost
  -> WindowMatcher
  -> Criterion
  -> Ability
reactionAbility entity idx cost window criteria =
  (mkAbility entity idx (ReactionAbility window cost))
    { abilityCriteria = criteria
    }

forcedAbility :: Sourceable a => a -> Int -> WindowMatcher -> Ability
forcedAbility entity idx window =
  mkAbility entity idx (ForcedAbility window)

restricted :: Criterion -> Ability -> Ability
restricted criteria = abilityCriteriaL .~ criteria

withCriteria :: Ability -> Criterion -> Ability
withCriteria a c = a & abilityCriteriaL <>~ c

haunted :: Sourceable a => Text -> a -> Int -> Ability
haunted tooltip a n = withTooltip tooltip $ mkAbility a n Haunted

cosmos :: Sourceable a => a -> Int -> Ability
cosmos a n = mkAbility a n Cosmos

reaction
  :: Sourceable a => a -> Int -> Criterion -> Cost -> WindowMatcher -> Ability
reaction a n c cost wm = restrictedAbility a n c (ReactionAbility wm cost)

uncancellable :: Ability -> Ability
uncancellable ab = ab {abilityCanBeCancelled = False}

abilityEffect :: Sourceable a => a -> Cost -> Ability
abilityEffect a cost = mkAbility a (-1) (AbilityEffect cost)

mkAbility :: Sourceable a => a -> Int -> AbilityType -> Ability
mkAbility entity idx type' =
  Ability
    { abilitySource = toSource entity
    , abilityIndex = idx
    , abilityType = type'
    , abilityLimit = defaultAbilityLimit type'
    , abilityWindow = defaultAbilityWindow type'
    , abilityMetadata = Nothing
    , abilityCriteria = NoRestriction
    , abilityDoesNotProvokeAttacksOfOpportunity = False
    , abilityTooltip = Nothing
    , abilityCanBeCancelled = True
    , abilityDisplayAsAction = False
    }

applyAbilityModifiers :: Ability -> [ModifierType] -> Ability
applyAbilityModifiers a@Ability {abilityType} modifiers =
  a {abilityType = applyAbilityTypeModifiers abilityType modifiers}

overrideAbilityCriteria :: CriteriaOverride -> Ability -> Ability
overrideAbilityCriteria (CriteriaOverride override) ab =
  ab {abilityCriteria = override}

isSilentForcedAbility :: Ability -> Bool
isSilentForcedAbility Ability {abilityType} =
  isSilentForcedAbilityType abilityType

isReactionAbility :: Ability -> Bool
isReactionAbility Ability {abilityType} = isReactionAbilityType abilityType

isFastAbility :: Ability -> Bool
isFastAbility Ability {abilityType} = isFastAbilityType abilityType

isActionAbility :: Ability -> Bool
isActionAbility Ability {abilityType} =
  isJust $ abilityTypeAction abilityType

isTriggeredAbility :: Ability -> Bool
isTriggeredAbility =
  or . sequence [isReactionAbility, isFastAbility, isActionAbility]

abilityTypeAction :: AbilityType -> Maybe Action
abilityTypeAction = \case
  FastAbility' _ mAction -> mAction
  ReactionAbility {} -> Nothing
  ActionAbility mAction _ -> mAction
  ActionAbilityWithSkill mAction _ _ -> mAction
  ActionAbilityWithBefore mAction _ _ -> mAction
  ForcedAbility _ -> Nothing
  SilentForcedAbility _ -> Nothing
  ForcedAbilityWithCost _ _ -> Nothing
  AbilityEffect _ -> Nothing
  Haunted -> Nothing
  Cosmos -> Nothing
  Objective aType -> abilityTypeAction aType
  ForcedWhen _ aType -> abilityTypeAction aType

abilityTypeCost :: AbilityType -> Cost
abilityTypeCost = \case
  FastAbility' cost _ -> cost
  ReactionAbility _ cost -> cost
  ActionAbility _ cost -> cost
  ActionAbilityWithSkill _ _ cost -> cost
  ActionAbilityWithBefore _ _ cost -> cost
  SilentForcedAbility _ -> Free
  ForcedAbility _ -> Free
  ForcedAbilityWithCost _ cost -> cost
  AbilityEffect cost -> cost
  Haunted -> Free
  Cosmos -> Free
  Objective aType -> abilityTypeCost aType
  ForcedWhen _ aType -> abilityTypeCost aType

applyAbilityTypeModifiers :: AbilityType -> [ModifierType] -> AbilityType
applyAbilityTypeModifiers aType modifiers = case aType of
  FastAbility' cost mAction -> FastAbility' (applyCostModifiers cost modifiers) mAction
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
  Cosmos -> Cosmos
  Objective aType' -> Objective $ applyAbilityTypeModifiers aType' modifiers
  ForcedWhen c aType' -> ForcedWhen c $ applyAbilityTypeModifiers aType' modifiers

applyCostModifiers :: Cost -> [ModifierType] -> Cost
applyCostModifiers = foldl' applyCostModifier

applyCostModifier :: Cost -> ModifierType -> Cost
applyCostModifier _ IgnoreAllCosts = Free
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
  FastAbility' {} -> FastPlayerWindow
  ActionAbility {} -> Matcher.DuringTurn You
  ActionAbilityWithBefore {} -> Matcher.DuringTurn You
  ActionAbilityWithSkill {} -> Matcher.DuringTurn You
  ForcedAbility window -> window
  SilentForcedAbility window -> window
  ForcedAbilityWithCost window _ -> window
  ReactionAbility window _ -> window
  AbilityEffect _ -> AnyWindow
  Haunted -> AnyWindow
  Cosmos -> AnyWindow
  Objective aType -> defaultAbilityWindow aType
  ForcedWhen _ aType -> defaultAbilityWindow aType

isFastAbilityType :: AbilityType -> Bool
isFastAbilityType = \case
  FastAbility' {} -> True
  ForcedAbility {} -> False
  SilentForcedAbility {} -> False
  ForcedAbilityWithCost {} -> False
  Objective aType -> isFastAbilityType aType
  ReactionAbility {} -> False
  ActionAbility {} -> False
  ActionAbilityWithSkill {} -> False
  ActionAbilityWithBefore {} -> False
  AbilityEffect {} -> False
  Haunted {} -> False
  Cosmos {} -> False
  ForcedWhen _ aType -> isFastAbilityType aType

isReactionAbilityType :: AbilityType -> Bool
isReactionAbilityType = \case
  SilentForcedAbility {} -> False
  ForcedAbility {} -> False
  ForcedAbilityWithCost {} -> False
  Objective aType -> isReactionAbilityType aType
  FastAbility' {} -> False
  ReactionAbility {} -> True
  ActionAbility {} -> False
  ActionAbilityWithSkill {} -> False
  ActionAbilityWithBefore {} -> False
  AbilityEffect {} -> False
  Haunted {} -> False
  Cosmos {} -> False
  ForcedWhen _ aType -> isReactionAbilityType aType

isSilentForcedAbilityType :: AbilityType -> Bool
isSilentForcedAbilityType = \case
  SilentForcedAbility {} -> True
  ForcedAbility {} -> False
  ForcedAbilityWithCost {} -> False
  Objective aType -> isSilentForcedAbilityType aType
  FastAbility' {} -> False
  ReactionAbility {} -> False
  ActionAbility {} -> False
  ActionAbilityWithSkill {} -> False
  ActionAbilityWithBefore {} -> False
  AbilityEffect {} -> False
  Haunted {} -> False
  Cosmos {} -> False
  ForcedWhen _ aType -> isSilentForcedAbilityType aType

isPerWindowLimit :: AbilityLimit -> Bool
isPerWindowLimit = \case
  GroupLimit l _ -> l == PerWindow
  PlayerLimit l _ -> l == PerWindow
  PerInvestigatorLimit l _ -> l == PerWindow
  MaxPer _ l _ -> l == PerWindow
  NoLimit -> False

defaultAbilityLimit :: AbilityType -> AbilityLimit
defaultAbilityLimit = \case
  ForcedAbility window' -> case window' of
    SkillTestResult {} -> PlayerLimit PerTestOrAbility 1
    _ -> GroupLimit PerWindow 1
  SilentForcedAbility _ -> GroupLimit PerWindow 1
  ForcedAbilityWithCost _ _ -> GroupLimit PerWindow 1
  ReactionAbility _ _ -> PlayerLimit PerWindow 1
  FastAbility' {} -> NoLimit
  ActionAbility _ _ -> NoLimit
  ActionAbilityWithBefore {} -> NoLimit
  ActionAbilityWithSkill {} -> NoLimit
  AbilityEffect _ -> NoLimit
  Objective aType -> defaultAbilityLimit aType
  Haunted -> NoLimit
  Cosmos -> NoLimit
  ForcedWhen _ aType -> defaultAbilityLimit aType
