module Arkham.Types.Ability where

import Arkham.Json
import Arkham.Types.Action hiding (Ability)
import Arkham.Types.InvestigatorId
import Arkham.Types.Source
import Arkham.Types.Window
import ClassyPrelude

data AbilityType
  = FastAbility Window
  | ReactionAbility Window
  | ActionAbility Int (Maybe Action)
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

data AbilityLimit
  = NoLimit
  | PerGame
  | PerPhase
  | PerRound
  | PerTurn
  | PerTestOrAbility
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

mkAbility :: Source -> Int -> AbilityType -> Ability
mkAbility source idx type' = Ability
  { abilitySource = source
  , abilityProvider = source
  , abilityIndex = idx
  , abilityType = type'
  , abilityLimit = NoLimit
  }

data Ability = Ability
  { abilitySource :: Source
  , abilityProvider :: Source
  , abilityIndex :: Int
  , abilityType :: AbilityType
  , abilityLimit :: AbilityLimit
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
