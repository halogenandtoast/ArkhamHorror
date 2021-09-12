module Arkham.Types.Ability
  ( module Arkham.Types.Ability
  , module X
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability.Limit as X
import Arkham.Types.Ability.Type as X
import Arkham.Types.Action (Action)
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes.Entity.Source
import Arkham.Types.Cost
import Arkham.Types.Criteria (Criterion)
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

data Ability = Ability
  { abilitySource :: Source
  , abilityIndex :: Int
  , abilityType :: AbilityType
  , abilityLimit :: AbilityLimit
  , abilityWindow :: WindowMatcher
  , abilityMetadata :: Maybe AbilityMetadata
  , abilityCriteria :: Maybe Criterion
  , abilityDoesNotProvokeAttacksOfOpportunity :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass Hashable

abilityCost :: Ability -> Cost
abilityCost = abilityTypeCost . abilityType

abilityAction :: Ability -> Maybe Action
abilityAction = abilityTypeAction . abilityType

abilityIs :: Ability -> Action -> Bool
abilityIs a = (== abilityAction a) . Just

abilityLimitL :: Lens' Ability AbilityLimit
abilityLimitL = lens abilityLimit $ \m x -> m { abilityLimit = x }

abilityMetadataL :: Lens' Ability (Maybe AbilityMetadata)
abilityMetadataL = lens abilityMetadata $ \m x -> m { abilityMetadata = x }

abilityDoesNotProvokeAttacksOfOpportunityL :: Lens' Ability Bool
abilityDoesNotProvokeAttacksOfOpportunityL =
  lens abilityDoesNotProvokeAttacksOfOpportunity
    $ \m x -> m { abilityDoesNotProvokeAttacksOfOpportunity = x }

instance Eq Ability where
  a == b =
    (abilitySource a == abilitySource b) && (abilityIndex a == abilityIndex b)

instance ToJSON Ability where
  toJSON = genericToJSON $ aesonOptions $ Just "ability"
  toEncoding = genericToEncoding $ aesonOptions $ Just "ability"

instance FromJSON Ability where
  parseJSON = genericParseJSON $ aesonOptions $ Just "ability"

newtype UsedAbility = UsedAbility { unUsedAbility :: (InvestigatorId, Ability) }

data AbilityMetadata
  = IntMetadata Int
  | TargetMetadata Target
  | SourceMetadata Source
  | EncounterCardMetadata EncounterCard
  | SkillChoiceMetadata SkillType
  | NoAbilityMetadata
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

restrictedAbility
  :: SourceEntity a => a -> Int -> Criterion -> AbilityType -> Ability
restrictedAbility entity idx restriction type' =
  (mkAbility entity idx type') { abilityCriteria = Just restriction }

abilityEffect :: SourceEntity a => a -> Cost -> Ability
abilityEffect a cost = mkAbility a (-1) (AbilityEffect cost)

defaultAbilityLimit :: AbilityType -> AbilityLimit
defaultAbilityLimit = \case
  ForcedAbility _ -> PlayerLimit PerWindow 1
  ForcedAbilityWithCost _ _ -> PlayerLimit PerWindow 1
  LegacyForcedAbility -> PlayerLimit PerWindow 1
  ReactionAbility _ _ -> PlayerLimit PerWindow 1
  LegacyReactionAbility _ -> PlayerLimit PerWindow 1
  FastAbility _ -> NoLimit
  ActionAbility _ _ -> NoLimit
  ActionAbilityWithBefore{} -> NoLimit
  ActionAbilityWithSkill{} -> NoLimit
  AbilityEffect _ -> NoLimit
  Objective aType -> defaultAbilityLimit aType

defaultAbilityWindow :: AbilityType -> WindowMatcher
defaultAbilityWindow = \case
  FastAbility _ -> FastPlayerWindow
  ActionAbility{} -> DuringTurn You
  ActionAbilityWithBefore{} -> DuringTurn You
  ActionAbilityWithSkill{} -> DuringTurn You
  ForcedAbility window -> window
  ForcedAbilityWithCost window _ -> window
  LegacyForcedAbility -> AnyWindow
  ReactionAbility window _ -> window
  LegacyReactionAbility _ -> AnyWindow
  AbilityEffect _ -> AnyWindow
  Objective aType -> defaultAbilityWindow aType

mkAbility :: SourceEntity a => a -> Int -> AbilityType -> Ability
mkAbility entity idx type' = Ability
  { abilitySource = toSource entity
  , abilityIndex = idx
  , abilityType = type'
  , abilityLimit = defaultAbilityLimit type'
  , abilityWindow = defaultAbilityWindow type'
  , abilityMetadata = Nothing
  , abilityCriteria = Nothing
  , abilityDoesNotProvokeAttacksOfOpportunity = False
  }

applyAbilityModifiers :: Ability -> [ModifierType] -> Ability
applyAbilityModifiers a@Ability { abilityType } modifiers =
  a { abilityType = applyAbilityTypeModifiers abilityType modifiers }

isForcedAbility :: Ability -> Bool
isForcedAbility Ability { abilityType } = go abilityType
 where
  go = \case
    LegacyForcedAbility -> True
    ForcedAbility{} -> True
    ForcedAbilityWithCost{} -> True
    Objective aType -> go aType
    FastAbility{} -> False
    LegacyReactionAbility{} -> False
    ReactionAbility{} -> False
    ActionAbility{} -> False
    ActionAbilityWithSkill{} -> False
    ActionAbilityWithBefore{} -> False
    AbilityEffect{} -> False

isFastAbility :: Ability -> Bool
isFastAbility Ability { abilityType } = go abilityType
 where
  go = \case
    FastAbility{} -> True
    LegacyForcedAbility -> False
    ForcedAbility{} -> False
    ForcedAbilityWithCost{} -> False
    Objective aType -> go aType
    LegacyReactionAbility{} -> False
    ReactionAbility{} -> False
    ActionAbility{} -> False
    ActionAbilityWithSkill{} -> False
    ActionAbilityWithBefore{} -> False
    AbilityEffect{} -> False
