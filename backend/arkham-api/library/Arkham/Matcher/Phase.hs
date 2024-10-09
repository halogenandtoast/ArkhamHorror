{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Phase where

import Arkham.Prelude
import Data.Aeson.TH
import GHC.OverloadedLabels

data PhaseMatcher = AnyPhase | IsMythosPhase | IsEnemyPhase | IsInvestigationPhase | IsUpkeepPhase
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "any" PhaseMatcher where
  fromLabel = AnyPhase

instance IsLabel "mythos" PhaseMatcher where
  fromLabel = IsMythosPhase

instance IsLabel "upkeep" PhaseMatcher where
  fromLabel = IsUpkeepPhase

instance IsLabel "enemy" PhaseMatcher where
  fromLabel = IsEnemyPhase

instance IsLabel "investigation" PhaseMatcher where
  fromLabel = IsInvestigationPhase

data PhaseStepMatcher = EnemiesAttackStep | HuntersMoveStep
  deriving stock (Show, Eq, Ord, Data)

data WindowMythosStepMatcher
  = WhenAllDrawEncounterCard
  | AfterCheckDoomThreshold
  deriving stock (Show, Eq, Ord, Data)

mconcat
  [ deriveJSON defaultOptions ''PhaseMatcher
  , deriveJSON defaultOptions ''PhaseStepMatcher
  , deriveJSON defaultOptions ''WindowMythosStepMatcher
  ]
