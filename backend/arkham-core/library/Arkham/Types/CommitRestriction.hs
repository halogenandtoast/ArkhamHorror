module Arkham.Types.CommitRestriction where

import Arkham.Prelude

data CommitRestriction
  = MaxOnePerTest
  | OnlyYourTest
  | OnlyIfYourLocationHasClues
  | ScenarioAbility
  | MinSkillTestValueDifference Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
