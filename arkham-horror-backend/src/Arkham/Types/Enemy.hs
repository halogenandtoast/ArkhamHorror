module Arkham.Types.Enemy where

import Arkham.Types.Card
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing
import Data.UUID

data ArkhamEnemy = ArkhamEnemy
  { _enemyId :: UUID
  , _enemyCombat :: Int
  , _enemyHealth :: Int
  , _enemyDamage :: Int
  , _enemyAgility :: Int
  , _enemyHealthDamage :: Int
  , _enemySanityDamage :: Int
  , _enemyVictory :: Maybe Int
  , _enemyCardCode :: ArkhamCardCode
  , _enemyIsHunter :: Bool
  , _enemyIsEngaged :: Bool
  , _enemyImage :: String
  , _enemyTraits :: [ArkhamTrait]
  , _enemyFinishedAttacking :: Bool
  }
  deriving stock (Show, Generic)

instance ToJSON ArkhamEnemy where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 6 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 6 }

instance FromJSON ArkhamEnemy where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 6 }
