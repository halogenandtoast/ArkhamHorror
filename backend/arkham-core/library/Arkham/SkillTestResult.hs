module Arkham.SkillTestResult where

import Arkham.Prelude

data SkillTestResult
  = Unrun
  | SucceededBy Bool Int
  | FailedBy Bool Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
