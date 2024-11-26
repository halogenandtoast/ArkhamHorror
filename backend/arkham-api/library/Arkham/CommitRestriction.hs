{-# LANGUAGE TemplateHaskell #-}

module Arkham.CommitRestriction where

import Arkham.Action (Action)
import Arkham.Matcher.Enemy
import Arkham.Matcher.Investigator
import Arkham.Prelude
import Data.Aeson.TH

data CommitRestriction
  = MaxOnePerTest
  | OnlyYourTest
  | OnlyNotYourTest
  | OnlyTestWithActions [Action]
  | OnlyIfYourLocationHasClues
  | ScenarioAbility
  | MinSkillTestValueDifference Int
  | SelfCanCommitWhen InvestigatorMatcher
  | CommittableTreachery
  | OnlyCardCommittedToTest
  | MustBeCommittedToYourTest
  | OnlyInvestigator InvestigatorMatcher
  | OnlyTestDuringYourTurn
  | OnlyFightAgainst EnemyMatcher
  | OnlyEvasionAgainst EnemyMatcher
  | AnyCommitRestriction [CommitRestriction]
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''CommitRestriction)
