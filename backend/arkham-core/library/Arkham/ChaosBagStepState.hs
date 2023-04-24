{-# LANGUAGE TemplateHaskell #-}
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
import Arkham.Source
import Data.Aeson.TH

data ChaosBagStepState
  = Resolved { tokens :: [Token] }
  | Decided { step :: ChaosBagStep }
  | Undecided { step :: ChaosBagStep }
  | Deciding { step :: ChaosBagStep }
  deriving stock (Show, Eq, Ord)

data TokenStrategy = ResolveChoice | CancelChoice | IgnoreChoice
  deriving stock (Show, Eq, Ord)

data ChaosBagStep
  = Draw
  | Choose
    { source :: Source
    , amount :: Int
    , tokenStrategy :: TokenStrategy
    , steps :: [ChaosBagStepState]
    , tokenGroups :: [[Token]]
    }
  | ChooseMatch
    { source :: Source
    , amount :: Int
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
  deriving stock (Show, Eq, Ord)

$(do
    tokenStrategy <- deriveJSON defaultOptions ''TokenStrategy
    chaosBagStep <- deriveJSON defaultOptions ''ChaosBagStep
    chaosBagStepState <- deriveJSON defaultOptions ''ChaosBagStepState
    pure $ concat [tokenStrategy, chaosBagStep, chaosBagStepState]
  )
