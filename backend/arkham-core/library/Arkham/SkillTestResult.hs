module Arkham.SkillTestResult where

import Arkham.Prelude

-- TODO: figure out what the bool is for
data SkillTestResult
  = Unrun
  | SucceededBy Bool Int
  | FailedBy Bool Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
