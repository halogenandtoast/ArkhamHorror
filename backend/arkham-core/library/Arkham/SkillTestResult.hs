{-# LANGUAGE TemplateHaskell #-}

module Arkham.SkillTestResult where

import Arkham.Prelude

import Data.Aeson.TH

data SkillTestResultType = Automatic | NonAutomatic
  deriving stock (Show, Eq, Ord)

data SkillTestResult
  = Unrun
  | SucceededBy SkillTestResultType Int
  | FailedBy SkillTestResultType Int
  deriving stock (Show, Eq, Ord)

$(deriveJSON defaultOptions ''SkillTestResultType)
$(deriveJSON defaultOptions ''SkillTestResult)
