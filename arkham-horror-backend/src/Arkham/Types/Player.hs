module Arkham.Types.Player
  ( ArkhamPlayer(..)
  )
where

import Arkham.Types.Asset
import Arkham.Types.Card
import Arkham.Types.Investigator
import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing
import Data.UUID

data ArkhamPlayer = ArkhamPlayer
  { _investigator :: ArkhamInvestigator
  , _sanityDamage :: Int
  , _healthDamage :: Int
  , _resources :: Int
  , _clues :: Int
  , _hand :: [ArkhamCard]
  , _assets :: HashMap UUID ArkhamAsset
  , _deck :: [ArkhamCard]
  , _discard :: [ArkhamCard]
  , _enemies :: HashSet UUID
  , _actionsRemaining :: Int
  , _endedTurn :: Bool
  , _accessibleLocations :: [ArkhamCardCode]
  , _playerId :: UUID
  }
  deriving stock (Generic, Show)

instance Eq ArkhamPlayer where
  p1 == p2 = _playerId p1 == _playerId p2

instance ToJSON ArkhamPlayer where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 1 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 1 }

instance FromJSON ArkhamPlayer where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 1 }
