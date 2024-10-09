module Arkham.Matcher.Investigator where

import Arkham.Prelude

type Who = InvestigatorMatcher
data InvestigatorMatcher

instance Show InvestigatorMatcher
instance Eq InvestigatorMatcher
instance Ord InvestigatorMatcher
instance Data InvestigatorMatcher

instance ToJSON InvestigatorMatcher
instance FromJSON InvestigatorMatcher
