module Arkham.Types.ChaosBagStepState
  ( ChaosBagStepState(..)
  , ChaosBagStep(..)
  )
where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Token

data ChaosBagStepState = Resolved [Token] | Decided ChaosBagStep | Undecided ChaosBagStep
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ChaosBagStep = Draw | Choose Int [ChaosBagStepState] [[Token]]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
