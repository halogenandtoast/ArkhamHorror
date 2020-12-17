module Arkham.Types.CommitRestriction where

import Arkham.Prelude

data CommitRestriction = MaxOnePerTest | OnlyYourTest | ScenarioAbility
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
