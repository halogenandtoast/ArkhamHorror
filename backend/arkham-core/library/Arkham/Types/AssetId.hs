module Arkham.Types.AssetId where

import Arkham.Prelude

import Arkham.Types.Card.Id

newtype AssetId = AssetId { unAssetId :: CardId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Random)

newtype ClosestAssetId = ClosestAssetId { unClosestAssetId :: AssetId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
