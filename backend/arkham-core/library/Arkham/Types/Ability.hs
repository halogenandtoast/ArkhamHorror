module Arkham.Types.Ability
  ( module Arkham.Types.Ability
  , module X
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability.Limit as X
import Arkham.Types.Ability.Type as X
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes.Entity.Source
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target

data Ability = Ability
  { abilitySource :: Source
  , abilityIndex :: Int
  , abilityType :: AbilityType
  , abilityLimit :: AbilityLimit
  , abilityMetadata :: Maybe AbilityMetadata
  , abilityRestrictions :: Maybe AbilityRestriction
  }
  deriving stock (Show, Generic)

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

data AbilityMetadata = IntMetadata Int | TargetMetadata Target | SourceMetadata Source | EncounterCardMetadata EncounterCard
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data AbilityRestriction
  = InvestigatorOnLocation LocationId
  | OrAbilityRestrictions [AbilityRestriction]
  | InvestigatorIsAlone
  | InvestigatorIsOwner
  | InvestigatorNotEngaged
  | AllAbilityRestrictions [AbilityRestriction]
  | IsUnowned
  | InvestigatorOnLocationAdjacentTo LocationMatcher
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

mkAbility :: SourceEntity a => a -> Int -> AbilityType -> Ability
mkAbility entity idx type' = Ability
  { abilitySource = toSource entity
  , abilityIndex = idx
  , abilityType = type'
  , abilityLimit = if type' == ForcedAbility
    then PlayerLimit PerWindow 1
    else NoLimit
  , abilityMetadata = Nothing
  , abilityRestrictions = Nothing
  }

applyAbilityModifiers :: Ability -> [Modifier] -> Ability
applyAbilityModifiers a@Ability { abilityType } modifiers =
  a { abilityType = applyAbilityTypeModifiers abilityType modifiers }
