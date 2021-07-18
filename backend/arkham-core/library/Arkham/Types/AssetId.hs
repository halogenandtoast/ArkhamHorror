module Arkham.Types.AssetId where

import Arkham.Prelude

import Arkham.Types.Card.Id

newtype AssetId = AssetId { unAssetId :: CardId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, Random)

newtype ClosestAssetId = ClosestAssetId { unClosestAssetId :: AssetId }
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
