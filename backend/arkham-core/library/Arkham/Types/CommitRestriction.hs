module Arkham.Types.CommitRestriction where

import Arkham.Prelude

import Arkham.Types.Matcher

data CommitRestriction
  = MaxOnePerTest
  | OnlyYourTest
  | OnlyIfYourLocationHasClues
  | ScenarioAbility
  | MinSkillTestValueDifference Int
  | SelfCanCommitWhen InvestigatorMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
