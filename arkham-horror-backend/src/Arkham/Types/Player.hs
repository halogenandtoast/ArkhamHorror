module Arkham.Types.Player where

import Arkham.Types.Card
import Arkham.Types.Investigator
import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing

data ArkhamPlayer = ArkhamPlayer
  { _investigator :: ArkhamInvestigator
  , _sanityDamage :: Int
  , _healthDamage :: Int
  , _resources :: Int
  , _clues :: Int
  , _hand :: [ArkhamCard]
  , _inPlay :: [ArkhamCard]
  , _deck :: [ArkhamCard]
  }
  deriving stock (Generic, Show)

instance ToJSON ArkhamPlayer where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 1 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 1 }

instance FromJSON ArkhamPlayer where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 1 }
