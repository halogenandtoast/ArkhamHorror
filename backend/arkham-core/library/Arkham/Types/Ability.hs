module Arkham.Types.Ability where

import Arkham.Json
import Arkham.Types.Action hiding (Ability)
import Arkham.Types.FastWindow
import Arkham.Types.Source
import ClassyPrelude

data AbilityType = FastAbility FastWindow | ReactionAbility FastWindow | ActionAbility Int (Maybe ActionType)
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

data AbilityLimit = NoLimit | OncePerRound | OncePerGame
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

data AbilityCost = ResourceCost Int | ClueCost Int | CardCost Int
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

mkAbility :: Source -> Int -> AbilityType -> Ability
mkAbility source idx type' = Ability
  { abilitySource = source
  , abilityProvider = source
  , abilityIndex = idx
  , abilityType = type'
  , abilityLimit = NoLimit
  , abilityCost = Nothing
  }

data Ability = Ability
  { abilitySource :: Source
  , abilityProvider :: Source
  , abilityIndex :: Int
  , abilityType :: AbilityType
  , abilityLimit :: AbilityLimit
  , abilityCost :: Maybe AbilityCost
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

newtype UsedAbility = UsedAbility { unUsedAbility :: Ability }
