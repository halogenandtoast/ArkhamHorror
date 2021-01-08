module Arkham.Types.Ability
  ( module Arkham.Types.Ability
  , module X
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability.Limit as X
import Arkham.Types.Ability.Type as X
import Arkham.Types.InvestigatorId
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Modifier

data Ability = Ability
  { abilitySource :: Source
  , abilityIndex :: Int
  , abilityType :: AbilityType
  , abilityLimit :: AbilityLimit
  , abilityMetadata :: Maybe AbilityMetadata
  }
  deriving stock (Show, Generic)

instance Eq Ability where
  a == b =
    (abilitySource a == abilitySource b) && (abilityIndex a == abilityIndex b)

instance ToJSON Ability where
  toJSON = genericToJSON $ aesonOptions $ Just "ability"
  toEncoding = genericToEncoding $ aesonOptions $ Just "ability"

instance FromJSON Ability where
  parseJSON = genericParseJSON $ aesonOptions $ Just "ability"

newtype UsedAbility = UsedAbility { unUsedAbility :: (InvestigatorId, Ability) }

data AbilityMetadata = IntMetadata Int | TargetMetadata Target | SourceMetadata Source
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

mkAbility :: Source -> Int -> AbilityType -> Ability
mkAbility source idx type' = Ability
  { abilitySource = source
  , abilityIndex = idx
  , abilityType = type'
  , abilityLimit = NoLimit
  , abilityMetadata = Nothing
  }

applyAbilityModifiers :: Ability -> [Modifier] -> Ability
applyAbilityModifiers a@Ability { abilityType } modifiers =
  a { abilityType = applyAbilityTypeModifiers abilityType modifiers }
