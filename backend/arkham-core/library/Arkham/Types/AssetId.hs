module Arkham.Types.AssetId where

import ClassyPrelude
import Data.Aeson
import Data.UUID

newtype AssetId = AssetId { unAssetId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype StoryAssetId = StoryAssetId { unStoryAssetId :: AssetId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype HealthDamageableAssetId = HealthDamageableAssetId { unHealthDamageableAssetId :: AssetId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype SanityDamageableAssetId = SanityDamageableAssetId { unSanityDamageableAssetId :: AssetId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype ExhaustedAssetId = ExhaustedAssetId { unExhaustedAssetId :: AssetId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype DiscardableAssetId = DiscardableAssetId { unDiscardableAssetId :: AssetId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
