module Arkham.Matcher.Types where

import Arkham.Prelude

data AbilityMatcher

instance ToJSON AbilityMatcher
instance FromJSON AbilityMatcher
instance Hashable AbilityMatcher
instance Show AbilityMatcher
instance Eq AbilityMatcher

data LocationMatcher

instance ToJSON LocationMatcher
instance FromJSON LocationMatcher
instance Hashable LocationMatcher
instance Show LocationMatcher
instance Eq LocationMatcher

data CardMatcher

instance ToJSON CardMatcher
instance FromJSON CardMatcher
instance Hashable CardMatcher
instance Show CardMatcher
instance Eq CardMatcher

data InvestigatorMatcher

instance ToJSON InvestigatorMatcher
instance FromJSON InvestigatorMatcher
instance Hashable InvestigatorMatcher
instance Show InvestigatorMatcher
instance Eq InvestigatorMatcher

data SourceMatcher

instance ToJSON SourceMatcher
instance FromJSON SourceMatcher
instance Hashable SourceMatcher
instance Show SourceMatcher
instance Eq SourceMatcher

data EnemyMatcher

instance ToJSON EnemyMatcher
instance FromJSON EnemyMatcher
instance Hashable EnemyMatcher
instance Show EnemyMatcher
instance Eq EnemyMatcher

data TokenMatcher

instance ToJSON TokenMatcher
instance FromJSON TokenMatcher
instance Hashable TokenMatcher
instance Show TokenMatcher
instance Eq TokenMatcher
