module Arkham.Matcher.Event where

import Arkham.Prelude

data EventMatcher

instance Show EventMatcher
instance Eq EventMatcher
instance Ord EventMatcher
instance Data EventMatcher

instance ToJSON EventMatcher
instance FromJSON EventMatcher
