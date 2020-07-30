module Arkham.Types.AssetId where

import ClassyPrelude
import Data.Aeson
import Data.UUID

newtype AssetId = AssetId { unAssetId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype StoryAssetId = StoryAssetId { unStoryAssetId :: AssetId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype DamageableAssetId = DamageableAssetId { unDamageableAssetId :: AssetId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
