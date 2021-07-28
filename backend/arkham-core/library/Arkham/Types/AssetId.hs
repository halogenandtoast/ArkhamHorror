module Arkham.Types.AssetId where

import Arkham.Prelude

import Arkham.Types.Card.Id

newtype AssetId = AssetId { unAssetId :: CardId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, Random)

newtype ClosestAssetId = ClosestAssetId { unClosestAssetId :: AssetId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
