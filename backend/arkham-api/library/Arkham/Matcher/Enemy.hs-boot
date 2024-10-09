module Arkham.Matcher.Enemy where

import Arkham.Prelude

data EnemyMatcher

instance Show EnemyMatcher
instance Eq EnemyMatcher
instance Ord EnemyMatcher
instance Data EnemyMatcher

instance ToJSON EnemyMatcher
instance FromJSON EnemyMatcher
