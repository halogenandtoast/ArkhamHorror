{-# LANGUAGE TemplateHaskell #-}

module Arkham.SkillTest.Step where

import Arkham.Prelude
import Data.Aeson.TH

data SkillTestStep
  = DetermineSkillOfTestStep -- ST.1
  | SkillTestFastWindow1
  | CommitCardsFromHandToSkillTestStep -- ST.2
  | SkillTestFastWindow2
  | RevealChaosTokenStep -- ST.3
  | ResolveChaosSymbolEffectsStep -- ST.4
  | DetermineInvestigatorsModifiedSkillValueStep -- ST.5
  | DetermineSuccessOrFailureOfSkillTestStep -- ST.6
  | ApplySkillTestResultsStep -- ST.7
  | SkillTestEndsStep -- ST.8
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''SkillTestStep)
