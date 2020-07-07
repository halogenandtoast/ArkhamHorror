module Arkham.Types.Asset
  ( ArkhamAsset(..)
  )
where

import Arkham.Types.Card
import ClassyPrelude
import Data.UUID
import Json

data ArkhamAsset = ArkhamAsset
  { aasName :: Text
  , aasCost :: Maybe Int
  , aasCode :: ArkhamCardCode
  , aasImage :: Text
  , aasUses :: Maybe Int
  , aasHasActionsAvailable :: Bool
  , aasAssetId :: UUID
  }
  deriving stock (Show, Generic)

instance FromJSON ArkhamAsset where
  parseJSON = genericParseJSON . aesonOptions $ Just "aas"

instance ToJSON ArkhamAsset where
  toJSON = genericToJSON . aesonOptions $ Just "aas"
  toEncoding = genericToEncoding . aesonOptions $ Just "aas"
