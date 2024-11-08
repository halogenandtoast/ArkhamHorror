module Arkham.Matcher.Investigator where

import Arkham.Id
import Arkham.Prelude

class IsInvestigatorMatcher a where
  toInvestigatorMatcher :: a -> InvestigatorMatcher

instance IsInvestigatorMatcher InvestigatorId

type Who = InvestigatorMatcher
data InvestigatorMatcher

instance Show InvestigatorMatcher
instance Eq InvestigatorMatcher
instance Ord InvestigatorMatcher
instance Data InvestigatorMatcher

instance ToJSON InvestigatorMatcher
instance FromJSON InvestigatorMatcher
