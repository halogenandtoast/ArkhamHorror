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
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

data Ability = Ability
  { abilitySource :: Source
  , abilityIndex :: Int
  , abilityType :: AbilityType
  , abilityLimit :: AbilityLimit
  , abilityMetadata :: Maybe AbilityMetadata
  , abilityRestrictions :: Maybe Restriction
  , abilityWindow :: WindowMatcher
  , abilityDoesNotProvokeAttacksOfOpportunity :: Bool
  }
  deriving stock (Show, Generic)

abilityCost :: Ability -> Cost
abilityCost = abilityTypeCost . abilityType

abilityAction :: Ability -> Maybe Action
abilityAction = abilityTypeAction . abilityType

abilityLimitL :: Lens' Ability AbilityLimit
abilityLimitL = lens abilityLimit $ \m x -> m { abilityLimit = x }

abilityMetadataL :: Lens' Ability (Maybe AbilityMetadata)
abilityMetadataL = lens abilityMetadata $ \m x -> m { abilityMetadata = x }

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
  | SkillChoiceMetadata [SkillType]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

restrictedAbility
  :: SourceEntity a => a -> Int -> Restriction -> AbilityType -> Ability
restrictedAbility entity idx restriction type' =
  (mkAbility entity idx type') { abilityRestrictions = Just restriction }

abilityEffect :: SourceEntity a => a -> Cost -> Ability
abilityEffect a cost = mkAbility a (-1) (AbilityEffect cost)

defaultAbilityLimit :: AbilityType -> AbilityLimit
defaultAbilityLimit = \case
  ForcedAbility _ -> PlayerLimit PerWindow 1
  ReactionAbility _ _ -> PlayerLimit PerWindow 1
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
  ReactionAbility window _ -> window
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
  , abilityRestrictions = Nothing
  , abilityDoesNotProvokeAttacksOfOpportunity = False
  }

applyAbilityModifiers :: Ability -> [ModifierType] -> Ability
applyAbilityModifiers a@Ability { abilityType } modifiers =
  a { abilityType = applyAbilityTypeModifiers abilityType modifiers }
