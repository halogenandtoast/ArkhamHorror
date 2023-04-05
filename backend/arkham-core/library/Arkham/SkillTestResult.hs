module Arkham.SkillTestResult where

import Arkham.Prelude

data SkillTestResultType = Automatic | NonAutomatic
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data SkillTestResult
  = Unrun
  | SucceededBy SkillTestResultType Int
  | FailedBy SkillTestResultType Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
