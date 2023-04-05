{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Arkham.ChaosBagStepState
  ( ChaosBagStepState(..)
  , ChaosBagStep(..)
  , TokenStrategy(..)
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

data TokenStrategy = ResolveChoice | CancelChoice | IgnoreChoice
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ChaosBagStep
  = Draw
  | Choose
    { amount :: Int
    , tokenStrategy :: TokenStrategy
    , steps :: [ChaosBagStepState]
    , tokenGroups :: [[Token]]
    }
  | ChooseMatch
    { amount :: Int
    , tokenStrategy :: TokenStrategy
    , steps :: [ChaosBagStepState]
    , tokenGroups :: [[Token]]
    , tokenMatcher :: TokenMatcher
    }
  | ChooseMatchChoice
    { steps :: [ChaosBagStepState]
    , tokenGroups :: [[Token]]
    , tokenMatcherChoices :: [(TokenMatcher, (Text, ChaosBagStep))]
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
