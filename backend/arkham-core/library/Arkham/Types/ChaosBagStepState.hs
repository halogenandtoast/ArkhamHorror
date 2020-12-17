module Arkham.Types.ChaosBagStepState
  ( ChaosBagStepState(..)
  , ChaosBagStep(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Token

data ChaosBagStepState = Resolved [Token] | Decided ChaosBagStep | Undecided ChaosBagStep
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ChaosBagStep = Draw | Choose Int [ChaosBagStepState] [[Token]]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
