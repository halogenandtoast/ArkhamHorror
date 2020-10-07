module Arkham.Types.SkillTestResult where

import ClassyPrelude
import Data.Aeson

data SkillTestResult = Unrun | SucceededBy Bool Int | FailedBy Bool Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
