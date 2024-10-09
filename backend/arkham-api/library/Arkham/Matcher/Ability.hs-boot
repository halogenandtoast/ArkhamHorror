module Arkham.Matcher.Ability where

import Arkham.Prelude

data AbilityMatcher

instance Show AbilityMatcher
instance Eq AbilityMatcher
instance Ord AbilityMatcher
instance Data AbilityMatcher

instance ToJSON AbilityMatcher
instance FromJSON AbilityMatcher
