module Arkham.Types.AssetMatcher where

import Arkham.Prelude

import Arkham.Types.AssetId

data AssetMatcher
  = AssetWithTitle Text
  | AssetWithFullTitle Text Text
  | AssetWithId AssetId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
