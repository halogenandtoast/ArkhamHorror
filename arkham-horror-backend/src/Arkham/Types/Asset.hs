module Arkham.Types.Asset
  ( ArkhamAsset(..)
  )
where

import Arkham.Types.Card
import Arkham.Types.Trait
import ClassyPrelude
import Data.UUID
import Json

data ArkhamAsset = ArkhamAsset
  { _assetName :: Text
  , _assetCost :: Int
  , _assetCode :: ArkhamCardCode
  , _assetImage :: Text
  , _assetUses :: Maybe Int
  , _assetHasActionsAvailable :: Bool
  , _assetId :: UUID
  , _assetTraits :: HashSet ArkhamTrait
  }
  deriving stock (Show, Generic)

instance FromJSON ArkhamAsset where
  parseJSON = genericParseJSON . aesonOptions $ Just "_asset"

instance ToJSON ArkhamAsset where
  toJSON = genericToJSON . aesonOptions $ Just "_asset"
  toEncoding = genericToEncoding . aesonOptions $ Just "_asset"
