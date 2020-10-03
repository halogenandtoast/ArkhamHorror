module Arkham.Types.Ability
  ( Ability(..)
  , AbilityType(..)
  , AbilityLimit(..)
  , UsedAbility(..)
  , AbilityMetadata(..)
  , mkAbility
  )
where

import Arkham.Json
import Arkham.Types.Ability.Limit
import Arkham.Types.Ability.Type
import Arkham.Types.InvestigatorId
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude

data Ability = Ability
  { abilitySource :: Source
  , abilityProvider :: Source
  , abilityIndex :: Int
  , abilityType :: AbilityType
  , abilityLimit :: AbilityLimit
  , abilityMetadata :: Maybe AbilityMetadata
  }
  deriving stock (Show, Generic)

instance Eq Ability where
  a == b =
    (abilityProvider a == abilityProvider b)
      && (abilityIndex a == abilityIndex b)

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
  , abilityProvider = source
  , abilityIndex = idx
  , abilityType = type'
  , abilityLimit = NoLimit
  , abilityMetadata = Nothing
  }
