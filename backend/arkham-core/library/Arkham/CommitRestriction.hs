module Arkham.CommitRestriction where

import Arkham.Prelude

import Arkham.Matcher
import Arkham.Action (Action)

data CommitRestriction
  = MaxOnePerTest
  | OnlyYourTest
  | OnlyTestWithActions [Action]
  | OnlyIfYourLocationHasClues
  | ScenarioAbility
  | MinSkillTestValueDifference Int
  | SelfCanCommitWhen InvestigatorMatcher
  | CommittableTreachery
  | OnlyCardCommittedToTest
  | MustBeCommittedToYourTest
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
