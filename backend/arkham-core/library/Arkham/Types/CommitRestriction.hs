module Arkham.Types.CommitRestriction where

import Arkham.Json
import ClassyPrelude

data CommitRestriction = MaxOnePerTest | OnlyYourTest
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
