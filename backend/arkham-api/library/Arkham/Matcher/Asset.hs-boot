module Arkham.Matcher.Asset where

import Arkham.Prelude

data AssetMatcher

instance Show AssetMatcher
instance Eq AssetMatcher
instance Ord AssetMatcher
instance Data AssetMatcher

instance ToJSON AssetMatcher
instance FromJSON AssetMatcher
