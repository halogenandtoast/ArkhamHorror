{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.ChaosBagStepState (
  ChaosBagStepState (..),
  ChaosBagStep (..),
  ChaosTokenStrategy (..),
) where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Matcher
import Arkham.Source
import Data.Aeson.TH

data ChaosBagStepState
  = Resolved {tokens :: [ChaosToken]}
  | Decided {step :: ChaosBagStep}
  | Undecided {step :: ChaosBagStep}
  | Deciding {step :: ChaosBagStep}
  deriving stock (Show, Eq, Ord, Data)

data ChaosTokenStrategy = ResolveChoice | CancelChoice | IgnoreChoice
  deriving stock (Show, Eq, Ord, Data)

data ChaosBagStep
  = Draw
  | Choose
      { source :: Source
      , amount :: Int
      , tokenStrategy :: ChaosTokenStrategy
      , steps :: [ChaosBagStepState]
      , tokenGroups :: [[ChaosToken]]
      }
  | ChooseMatch
      { source :: Source
      , amount :: Int
      , tokenStrategy :: ChaosTokenStrategy
      , steps :: [ChaosBagStepState]
      , tokenGroups :: [[ChaosToken]]
      , tokenMatcher :: ChaosTokenMatcher
      }
  | ChooseMatchChoice
      { steps :: [ChaosBagStepState]
      , tokenGroups :: [[ChaosToken]]
      , tokenMatcherChoices :: [(ChaosTokenMatcher, (Text, ChaosBagStep))]
      }
  deriving stock (Show, Eq, Ord, Data)

$( do
    tokenStrategy <- deriveJSON defaultOptions ''ChaosTokenStrategy
    chaosBagStep <- deriveJSON defaultOptions ''ChaosBagStep
    chaosBagStepState <- deriveJSON defaultOptions ''ChaosBagStepState
    pure $ concat [tokenStrategy, chaosBagStep, chaosBagStepState]
 )
