{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Ability (
  module X,
  module Arkham.Ability,
) where

import Arkham.Prelude

import Arkham.Ability.Limit as X
import Arkham.Ability.Type as X
import Arkham.Ability.Types as X
import Arkham.Ability.Used as X
import Arkham.Cost as X
import Arkham.Criteria as X

import Arkham.Ability.Types qualified
import Arkham.Action
import Arkham.Card.CardCode
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Modifier
import Arkham.Source
import Control.Lens (over, set, transform)
import Data.Data.Lens (biplate)
import GHC.Records

withAdditionalCost :: Cost -> Ability -> Ability
withAdditionalCost c ab = ab {abilityAdditionalCosts = c : abilityAdditionalCosts ab}

inHandAbility :: Ability -> Bool
inHandAbility = inHandCriteria . abilityCriteria
 where
  inHandCriteria = \case
    InYourHand -> True
    Criteria xs -> any inHandCriteria xs
    AnyCriterion xs -> any inHandCriteria xs
    _ -> False

inDiscardAbility :: Ability -> Bool
inDiscardAbility = inDiscardCriteria . abilityCriteria
 where
  inDiscardCriteria = \case
    InYourDiscard -> True
    Criteria xs -> any inDiscardCriteria xs
    AnyCriterion xs -> any inDiscardCriteria xs
    _ -> False

abilityCost :: Ability -> Cost
abilityCost = abilityTypeCost . abilityType

abilityActions :: Ability -> [Action]
abilityActions = abilityTypeActions . abilityType

instance HasField "actions" Ability [Action] where
  getField = abilityActions

abilityIs :: Ability -> Action -> Bool
abilityIs a = (`elem` abilityActions a)

abilityIsActionAbility :: Ability -> Bool
abilityIsActionAbility a = case abilityType a of
  ActionAbility {} -> True
  ActionAbilityWithSkill {} -> True
  ActionAbilityWithBefore {} -> True
  _ -> False

abilityIsActivate :: Ability -> Bool
abilityIsActivate a = not (abilityIndex a >= 100 && abilityIndex a <= 102) && abilityIsActionAbility a

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
  :: (HasCardCode a, Sourceable a) => a -> Int -> Criterion -> AbilityType -> Ability
restrictedAbility entity idx restriction type' =
  (mkAbility entity idx type') {abilityCriteria = restriction}

restricted
  :: (HasCardCode a, Sourceable a) => a -> Int -> Criterion -> AbilityType -> Ability
restricted = restrictedAbility

controlledAbility
  :: (HasCardCode a, Sourceable a) => a -> Int -> Criterion -> AbilityType -> Ability
controlledAbility entity idx restriction = restrictedAbility entity idx (ControlsThis <> restriction)

fastAbility :: (HasCardCode a, Sourceable a) => a -> Int -> Cost -> Criterion -> Ability
fastAbility entity idx cost criteria =
  (mkAbility entity idx (FastAbility cost))
    { abilityCriteria = criteria
    }

fightAbility :: (HasCardCode a, Sourceable a) => a -> Int -> Cost -> Criterion -> Ability
fightAbility entity idx cost criteria =
  (mkAbility entity idx (fightAction cost))
    { abilityCriteria = criteria
    }

evadeAbility :: (Sourceable a, HasCardCode a) => a -> Int -> Cost -> Criterion -> Ability
evadeAbility entity idx cost criteria =
  (mkAbility entity idx (ActionAbility [#evade] cost))
    { abilityCriteria = criteria
    }

investigateAbility :: (Sourceable a, HasCardCode a) => a -> Int -> Cost -> Criterion -> Ability
investigateAbility entity idx cost criteria =
  (mkAbility entity idx (investigateAction cost))
    { abilityCriteria = criteria <> exists (YourLocation <> InvestigatableLocation)
    }

reactionAbility
  :: (Sourceable a, HasCardCode a)
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

forcedAbility :: (HasCardCode a, Sourceable a) => a -> Int -> WindowMatcher -> Ability
forcedAbility entity idx window =
  mkAbility entity idx (ForcedAbility window)

-- restricted :: Criterion -> Ability -> Ability
-- restricted criteria = abilityCriteriaL .~ criteria

withCriteria :: Ability -> Criterion -> Ability
withCriteria a c = a & abilityCriteriaL <>~ c

restrict :: Criterion -> Ability -> Ability
restrict = flip withCriteria

haunted :: (HasCardCode a, Sourceable a) => Text -> a -> Int -> Ability
haunted tooltip a n = withTooltip tooltip $ mkAbility a n Haunted

cosmos :: (HasCardCode a, Sourceable a) => a -> Int -> Ability
cosmos a n = mkAbility a n Cosmos

reaction
  :: (HasCardCode a, Sourceable a) => a -> Int -> Criterion -> Cost -> WindowMatcher -> Ability
reaction a n c cost wm = restrictedAbility a n c (ReactionAbility wm cost)

uncancellable :: Ability -> Ability
uncancellable ab = ab {abilityCanBeCancelled = False}

abilityEffect :: (HasCardCode a, Sourceable a) => a -> Cost -> Ability
abilityEffect a cost = mkAbility a (-1) (AbilityEffect cost)

basicAbility :: Ability -> Ability
basicAbility ab = ab {abilityBasic = True}

mkAbility :: (Sourceable a, HasCardCode a) => a -> Int -> AbilityType -> Ability
mkAbility entity idx type' =
  Ability
    { abilitySource = toSource entity
    , abilityRequestor = toSource entity
    , abilityTriggersSkillTest = abilityTypeTriggersSkillTest type'
    , abilityCardCode = toCardCode entity
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
    , abilityDelayAdditionalCosts = False
    , abilityBasic = False
    , abilityAdditionalCosts = []
    }

applyAbilityModifiers :: Ability -> [ModifierType] -> Ability
applyAbilityModifiers a@Ability {abilityType, abilityCriteria} modifiers =
  a
    { Arkham.Ability.Types.abilityType = applyAbilityTypeModifiers abilityType modifiers
    , Arkham.Ability.Types.abilityCriteria = applyAbilityCriteriaModifiers abilityCriteria modifiers
    }

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
  notNull $ abilityTypeActions abilityType

abilityTypeTriggersSkillTest :: AbilityType -> Bool
abilityTypeTriggersSkillTest = any (`elem` [#fight, #evade, #investigate, #circle]) . abilityTypeActions

isTriggeredAbility :: Ability -> Bool
isTriggeredAbility =
  or . sequence [isReactionAbility, isFastAbility, isActionAbility]

abilityTypeActions :: AbilityType -> [Action]
abilityTypeActions = \case
  FastAbility' _ actions -> actions
  ReactionAbility {} -> []
  CustomizationReaction {} -> []
  ConstantReaction {} -> []
  ActionAbility actions _ -> #activate : actions
  ActionAbilityWithSkill actions _ _ -> #activate : actions
  ActionAbilityWithBefore actions _ _ -> #activate : actions
  ForcedAbility _ -> []
  SilentForcedAbility _ -> []
  ForcedAbilityWithCost _ _ -> []
  AbilityEffect _ -> []
  Haunted -> []
  ServitorAbility action -> [action]
  Cosmos -> []
  Objective aType -> abilityTypeActions aType
  ForcedWhen _ aType -> abilityTypeActions aType

abilityTypeCost :: AbilityType -> Cost
abilityTypeCost = \case
  FastAbility' cost _ -> cost
  ReactionAbility _ cost -> cost
  CustomizationReaction _ _ cost -> cost
  ConstantReaction _ _ cost -> cost
  ActionAbility _ cost -> cost
  ActionAbilityWithSkill _ _ cost -> cost
  ActionAbilityWithBefore _ _ cost -> cost
  SilentForcedAbility _ -> Free
  ForcedAbility _ -> Free
  ForcedAbilityWithCost _ cost -> cost
  AbilityEffect cost -> cost
  Haunted -> Free
  Cosmos -> Free
  ServitorAbility _ -> Free
  Objective aType -> abilityTypeCost aType
  ForcedWhen _ aType -> abilityTypeCost aType

modifyCost :: (Cost -> Cost) -> AbilityType -> AbilityType
modifyCost f = \case
  FastAbility' cost mAction -> FastAbility' (f cost) mAction
  ReactionAbility window cost ->
    ReactionAbility window $ f cost
  CustomizationReaction label window cost ->
    CustomizationReaction label window $ f cost
  ConstantReaction label window cost ->
    ConstantReaction label window $ f cost
  ActionAbility mAction cost ->
    ActionAbility mAction $ f cost
  ActionAbilityWithSkill mAction skill cost ->
    ActionAbilityWithSkill mAction skill $ f cost
  ActionAbilityWithBefore mAction mBeforeAction cost ->
    ActionAbilityWithBefore mAction mBeforeAction
      $ f cost
  ForcedAbility window -> ForcedAbility window
  SilentForcedAbility window -> SilentForcedAbility window
  ForcedAbilityWithCost window cost ->
    ForcedAbilityWithCost window $ f cost
  AbilityEffect cost -> AbilityEffect cost -- modifiers don't yet apply here
  Haunted -> Haunted
  ServitorAbility action -> ServitorAbility action
  Cosmos -> Cosmos
  Objective aType' -> Objective $ modifyCost f aType'
  ForcedWhen c aType' -> ForcedWhen c $ modifyCost f aType'

applyAbilityTypeModifiers :: AbilityType -> [ModifierType] -> AbilityType
applyAbilityTypeModifiers aType modifiers = modifyCost (`applyCostModifiers` modifiers) aType

applyAbilityCriteriaModifiers :: Criterion -> [ModifierType] -> Criterion
applyAbilityCriteriaModifiers c modifiers = foldr applyCriterionModifier c modifiers
 where
  isLocationCheck = \case
    OnSameLocation -> True
    OnLocation _ -> True
    _ -> False
  replaceEngagementCheck = \case
    EnemyIsEngagedWith _ -> AnyEnemy
    other -> other
  handleEnemyCriterion = \case
    EnemyExists em -> EnemyExists $ over biplate (transform replaceEngagementCheck) em
    NotAttackingEnemy -> NotAttackingEnemy
    EnemyExistsAtAttachedLocation em -> EnemyExistsAtAttachedLocation $ over biplate (transform replaceEngagementCheck) em
    ThisEnemy em -> ThisEnemy $ over biplate (transform replaceEngagementCheck) em
    EnemyMatchesCriteria ems -> EnemyMatchesCriteria $ map handleEnemyCriterion ems
  handleEngagementCheck = \case
    EnemyCriteria ec -> EnemyCriteria $ handleEnemyCriterion ec
    other -> other
  k x y z = if x z then y else z
  applyCriterionModifier :: ModifierType -> Criterion -> Criterion
  applyCriterionModifier IgnoreOnSameLocation c' = overCriteria (k isLocationCheck NoRestriction) c'
  applyCriterionModifier IgnoreEngagementRequirement c' = overCriteria handleEngagementCheck c'
  applyCriterionModifier _ c' = c'

applyCostModifiers :: Cost -> [ModifierType] -> Cost
applyCostModifiers = foldl' applyCostModifier

applyCostModifier :: Cost -> ModifierType -> Cost
applyCostModifier _ IgnoreAllCosts = Free
applyCostModifier (ActionCost _) IgnoreActionCost = Free
applyCostModifier (ActionCost n) (ActionCostModifier m) =
  ActionCost (max 0 $ n + m)
applyCostModifier (ActionCost _) (ActionCostSetToModifier m) = ActionCost m
applyCostModifier (Costs xs) modifier = Costs $ map (`applyCostModifier` modifier) xs
applyCostModifier (NonBlankedCost x) modifier = NonBlankedCost $ x `applyCostModifier` modifier
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
  CustomizationReaction _ window _ -> window
  ConstantReaction _ window _ -> window
  AbilityEffect _ -> AnyWindow
  Haunted -> AnyWindow
  ServitorAbility _ -> Matcher.DuringTurn You
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
  CustomizationReaction {} -> False
  ConstantReaction {} -> False
  ActionAbility {} -> False
  ActionAbilityWithSkill {} -> False
  ActionAbilityWithBefore {} -> False
  AbilityEffect {} -> False
  Haunted {} -> False
  ServitorAbility {} -> False
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
  CustomizationReaction {} -> False
  ConstantReaction {} -> True
  ActionAbility {} -> False
  ActionAbilityWithSkill {} -> False
  ActionAbilityWithBefore {} -> False
  AbilityEffect {} -> False
  Haunted {} -> False
  ServitorAbility {} -> False
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
  CustomizationReaction {} -> False
  ConstantReaction {} -> False
  ActionAbility {} -> False
  ActionAbilityWithSkill {} -> False
  ActionAbilityWithBefore {} -> False
  AbilityEffect {} -> False
  Haunted {} -> False
  ServitorAbility {} -> False
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
  CustomizationReaction {} -> PlayerLimit PerWindow 1
  ConstantReaction {} -> PlayerLimit PerWindow 1
  FastAbility' {} -> NoLimit
  ActionAbility _ _ -> NoLimit
  ActionAbilityWithBefore {} -> NoLimit
  ActionAbilityWithSkill {} -> NoLimit
  AbilityEffect _ -> NoLimit
  Objective aType -> defaultAbilityLimit aType
  Haunted -> NoLimit
  ServitorAbility _ -> NoLimit
  Cosmos -> NoLimit
  ForcedWhen _ aType -> defaultAbilityLimit aType

decreaseAbilityActionCost :: Ability -> Int -> Ability
decreaseAbilityActionCost ab n =
  ab {Arkham.Ability.Types.abilityType = modifyCost (`decreaseActionCost` n) (abilityType ab)}
