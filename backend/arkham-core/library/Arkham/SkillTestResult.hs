{-# LANGUAGE TemplateHaskell #-}

module Arkham.SkillTestResult where

import Arkham.Prelude

import Data.Aeson.TH

data SkillTestResultType = Automatic | NonAutomatic
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks)

data SkillTestResult
  = Unrun
  | SucceededBy SkillTestResultType Int
  | FailedBy SkillTestResultType Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks)

$(deriveJSON defaultOptions ''SkillTestResultType)
$(deriveJSON defaultOptions ''SkillTestResult)
