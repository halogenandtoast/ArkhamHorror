{-# LANGUAGE TemplateHaskell #-}

module Arkham.CommitRestriction where

import Arkham.Prelude

import Arkham.Action (Action)
import Arkham.Matcher
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
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''CommitRestriction)
