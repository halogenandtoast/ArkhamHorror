module Arkham.CommitRestriction where

import Arkham.Prelude

import Arkham.Matcher

data CommitRestriction
  = MaxOnePerTest
  | OnlyYourTest
  | OnlyIfYourLocationHasClues
  | ScenarioAbility
  | MinSkillTestValueDifference Int
  | SelfCanCommitWhen InvestigatorMatcher
  | CommittableTreachery
  | OnlyCardCommittedToTest
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
