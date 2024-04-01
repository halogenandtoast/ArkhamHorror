{-# LANGUAGE TemplateHaskell #-}

module Arkham.SkillTest.Step where

import Arkham.Prelude
import Data.Aeson.TH

data SkillTestStep
  = DetermineSkillOfTestStep
  | CommitCardsFromHandToSkillTestStep
  | RevealChaosTokenStep
  | ResolveChaosSymbolEffectsStep
  | DetermineInvestigatorsModifiedSkillValueStep
  | DetermineSuccessOrFailureOfSkillTestStep
  | ApplySkillTestResultsStep
  | SkillTestEndsStep
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''SkillTestStep)
