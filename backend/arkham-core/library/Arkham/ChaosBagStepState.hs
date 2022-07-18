module Arkham.ChaosBagStepState
  ( ChaosBagStepState(..)
  , ChaosBagStep(..)
  ) where

import Arkham.Prelude

import Arkham.Token
import Arkham.Matcher

data ChaosBagStepState = Resolved [Token] | Decided ChaosBagStep | Undecided ChaosBagStep
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ChaosBagStep
  = Draw
  | Choose Int [ChaosBagStepState] [[Token]]
  | ChooseMatch Int [ChaosBagStepState] [[Token]] TokenMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
