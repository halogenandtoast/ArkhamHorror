{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Arkham.ChaosBagStepState
  ( ChaosBagStepState(..)
  , ChaosBagStep(..)
  ) where

import Arkham.Prelude

import Arkham.Token
import Arkham.Matcher

data ChaosBagStepState
  = Resolved { tokens :: [Token] }
  | Decided { step :: ChaosBagStep }
  | Undecided { step :: ChaosBagStep }
  | Deciding { step :: ChaosBagStep }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ChaosBagStep
  = Draw
  | Choose { amount :: Int, steps :: [ChaosBagStepState], tokenGroups :: [[Token]] }
  | ChooseMatch { amount :: Int, steps :: [ChaosBagStepState], tokenGroups :: [[Token]], tokenMatcher :: TokenMatcher }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
